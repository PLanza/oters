mod allocator;
mod errors;

use self::allocator::Allocator;
use self::errors::InterpretError::*;
use crate::export::{ExportFns, Value};
use crate::exprs::{BOpcode, Expr, UOpcode};
use crate::parser::ast::Pattern;

use std::collections::{HashMap, HashSet, VecDeque};
use std::vec;

use anyhow::Result;
use petgraph::graph::DiGraph;

pub struct Interpreter {
    allocator: Allocator,
    globals: HashMap<String, Expr>,
    streams: HashMap<String, Expr>,
    eval_order: Vec<String>,
    imports: ExportFns,
    store: Store,
    stream_outs: HashMap<String, Expr>,
}

// Potentially change locations to be shared references
#[derive(Clone, Debug)]
pub struct Store {
    pub(super) now: HashMap<u64, Expr>,
    pub(super) later: HashMap<u64, Expr>,
    pub(super) streams: HashMap<String, u64>,
}

impl Interpreter {
    pub fn new(exprs: Vec<(String, Expr)>, imports: ExportFns) -> Result<Self> {
        let mut interp = Interpreter {
            allocator: Allocator::new(),
            globals: HashMap::new(),
            streams: HashMap::new(),
            eval_order: Vec::new(),
            imports,
            store: Store::new(),
            stream_outs: HashMap::new(),
        };
        interp.init(exprs)?;

        Ok(interp)
    }

    fn init(&mut self, exprs: Vec<(String, Expr)>) -> Result<()> {
        let mut flow_graph = DiGraph::<String, ()>::new();

        for (id, expr) in exprs {
            // Reduce all the expressions
            let (e, s) = self.eval(expr.clone(), self.store.clone())?;

            // If the resulting value is a stream then add it to the stream flow graph
            if !matches!(e.is_stream(), None) {
                let current_index = flow_graph.add_node(id.clone());
                // Any streams used in the expression are added to current_deps
                for (stream, _) in &self.streams {
                    // If the current expression depends on another stream
                    if expr.clone().substitute(stream, &Expr::Unit).0 {
                        // Add a dependency
                        let dep_index = flow_graph
                            .node_indices()
                            .find(|i| &flow_graph[*i] == stream)
                            .unwrap();
                        flow_graph.update_edge(dep_index, current_index, ());
                    }
                }
                self.streams.insert(id.clone(), e.clone());
            }

            // Globals contain reduced expressions e.g. functions, values, streams, etc.
            self.globals.insert(id.clone(), e);
            self.store = s;
        }

        // Warning: Breaks with cycles though these will fail in type checking
        // Topological sort makes sure dependencies are evaluated before dependents
        self.eval_order = petgraph::algo::toposort(
            &flow_graph,
            Some(&mut petgraph::algo::DfsSpace::new(&flow_graph)),
        )
        .unwrap()
        .into_iter()
        .map(|i| flow_graph[i].clone())
        .collect();

        Ok(())
    }

    pub fn eval_step(&mut self) -> Result<()> {
        if self.streams.len() == 0 {
            return Ok(());
        }

        self.step()?;
        for stream in self.eval_order.clone() {
            // println!("{}: {}", stream, self.stream_outs.get(&stream).unwrap());
            // println!("{}", self.streams.get(&stream).unwrap());
            // println!("{:?}\n", self.store);
            let e = self.streams.get(&stream).unwrap().clone();

            let (e, s) = self.eval(e, self.store.clone())?;
            self.store = s;

            match self.store.streams.get(&stream) {
                Some(loc) => {
                    let replacement = e.replace_stream_loc(*loc).unwrap();
                    self.store.now.insert(*loc, replacement);
                }
                None => (),
            }

            self.streams.insert(stream, e);
        }
        Ok(())
    }

    pub fn eval_loop(&mut self) -> Result<()> {
        if self.streams.len() == 0 {
            return Ok(());
        }

        loop {
            self.step()?;
            for stream in self.eval_order.clone() {
                // println!("{}: {}", stream, self.stream_outs.get(&stream).unwrap());
                // println!("{}", self.streams.get(&stream).unwrap());
                // println!("{:?}\n", self.store);
                let e = self.streams.get(&stream).unwrap().clone();

                let (e, s) = self.eval(e, self.store.clone())?;
                self.store = s;

                match self.store.streams.get(&stream) {
                    Some(loc) => {
                        let replacement = e.replace_stream_loc(*loc).unwrap();
                        self.store.now.insert(*loc, replacement);
                    }
                    None => (),
                }

                self.streams.insert(stream, e);
            }
        }
    }

    pub fn eval(&mut self, e: Expr, s: Store) -> Result<(Expr, Store)> {
        use Expr::*;
        match e {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) | Location(_) => Ok((e, s)),
            BinOp(e1, op, e2) => self.eval_bop(*e1, op, *e2, s),
            UnOp(UOpcode::Neg, e) => {
                let (_e, _s) = self.eval(*e.clone(), s)?;
                match _e {
                    Int(v) => Ok((Int(-v), _s)),
                    Float(v) => Ok((Float(-v), _s)),
                    _ => Err(UncaughtTypeError(format!("{:?}", e)).into()),
                }
            }
            UnOp(UOpcode::Not, e) => {
                let (_e, _s) = self.eval(*e.clone(), s)?;
                match _e {
                    Bool(v) => Ok((Bool(!v), _s)),
                    _ => Err(UncaughtTypeError(format!("{:?}", e)).into()),
                }
            }
            Delay(e) => {
                let loc = self.allocator.alloc();
                let mut s = s.clone();
                s.extend(loc, *e);
                Ok((Location(loc), s))
            }
            Stable(e) => Ok((Stable(e), s)),
            Adv(e) => {
                let s_n = s.now();
                let (_e, _s_n) = self.eval(*e.clone(), s_n)?;

                match _e {
                    Location(l) => match _s_n.now.get(&l) {
                        None => Err(UnboundLocationError(format!("{:?}", e)).into()),
                        Some(term) => self.eval(
                            term.clone(),
                            Store {
                                now: _s_n.now,
                                later: s.later,
                                streams: _s_n.streams,
                            },
                        ),
                    },
                    _ => Err(UncaughtTypeError(format!("HERE{:?}", e)).into()),
                }
            }
            Unbox(e) => {
                let (_e, _s) = self.eval(*e.clone(), s)?;
                match _e {
                    Stable(term) => self.eval(*term, _s),
                    _ => Err(UncaughtTypeError(format!("{:?}", e)).into()),
                }
            }
            Out(e) => {
                let (_e, _s) = self.eval(*e.clone(), s)?;
                match _e {
                    Into(value) => Ok((*value, _s)),
                    _ => Err(UncaughtTypeError(format!("{:?}", e)).into()),
                }
            }
            Into(e) => {
                let (value, _s) = self.eval(*e.clone(), s)?;
                Ok((Into(Box::new(value)), _s))
            }
            List(list) => {
                let mut list_vec = VecDeque::new();
                let mut store = s.clone();

                for e in list {
                    let (_e, _s) = self.eval(*e.clone(), store.clone())?;
                    list_vec.push_back(Box::new(_e));
                    store = _s;
                }

                Ok((List(list_vec), store))
            }
            Tuple(tuple) => {
                let mut tuple_vec = Vec::new();
                let mut store = s.clone();

                for e in tuple {
                    let (_e, _s) = self.eval(*e, store.clone())?;
                    tuple_vec.push(Box::new(_e));
                    store = _s;
                }

                Ok((Tuple(tuple_vec), store))
            }
            Struct(str, fields) => {
                let mut field_vec = Vec::new();
                let mut store = s.clone();

                for (f, e) in fields {
                    let (_e, _s) = self.eval(*e, store.clone())?;
                    field_vec.push((f, Box::new(_e)));
                    store = _s;
                }

                Ok((Struct(str, field_vec), store))
            }
            Variant(constr, opt) => match opt {
                None => Ok((Variant(constr, opt), s)),
                Some(e) => {
                    let (val, _s) = self.eval(*e.clone(), s)?;
                    Ok((Variant(constr, Some(Box::new(val))), _s))
                }
            },
            Fn(..) => Ok((e, s)),
            Fix(var, expr) => self.eval(
                expr.clone()
                    .substitute(
                        &var.clone(),
                        &Stable(Box::new(Delay(Box::new(Fix(var, expr))))),
                    )
                    .1,
                s,
            ),
            If(e1, e2, e3) => {
                let (_e1, _s) = self.eval(*e1.clone(), s)?;
                match _e1 {
                    Bool(true) => self.eval(*e2, _s),
                    Bool(false) => self.eval(*e3, _s),
                    _ => Err(UncaughtTypeError(format!("{:?}", e1)).into()),
                }
            }
            Seq(e1, e2) => {
                let (_e1, _s) = self.eval(*e1.clone(), s)?;
                match _e1 {
                    Unit => self.eval(*e2, _s),
                    _ => Err(UncaughtTypeError(format!("{:?}", e1)).into()),
                }
            }
            App(e1, e2) => {
                match &*e1 {
                    Var(var) => {
                        // If the function has been imported, call it with converted arguments
                        if self.imports.contains_key(var) {
                            let (func, arg_ts, _) = self.imports.get(var).unwrap().clone();
                            let (val, _s) = self.eval(*e2, s)?;
                            let ret_val = func(Value::expr_to_args(val, arg_ts.len())?);
                            return Ok((ret_val.to_expr(), _s));
                        }
                    }
                    _ => (),
                }

                let (_e1, _s) = self.eval(*e1.clone(), s)?;
                match _e1 {
                    Fn((var, _), expr) => {
                        let (val, __s) = self.eval(*e2, _s)?;
                        self.eval(expr.substitute(&var, &val).1, __s)
                    }
                    _ => Err(UncaughtTypeError(format!("{:?}", e1)).into()),
                }
            }
            ProjStruct(e, field) => {
                let (_e, _s) = self.eval(*e.clone(), s)?;
                match _e {
                    Struct(_, fields) => {
                        for (f, val) in fields {
                            if f == field {
                                return Ok((*val, _s));
                            }
                        }
                        Err(StructFieldMissingError(field, format!("{:?}", e)).into())
                    }
                    _ => Err(UncaughtTypeError(format!("{:?}", e)).into()),
                }
            }
            Match(e, patterns) => {
                let (val, _s) = self.eval(*e.clone(), s)?;
                for (p, e_) in patterns {
                    let (matches, subs) = Self::match_pattern(&val, &p)?;
                    if !matches {
                        continue;
                    }

                    let mut e_ = *e_.clone();
                    for (var, term) in subs {
                        e_ = e_.substitute(&var, &term).1;
                    }
                    return self.eval(e_, _s);
                }
                Err(NoPatternMatchesError(format!("{:?}", e)).into())
            }
            Var(x) => {
                let opt = self.streams.get(&x).map(|e| e.clone());
                match opt {
                    None => match self.globals.get(&x) {
                        None => Err(UnboundVariableError(x).into()),
                        Some(v) => Ok((v.clone(), s)),
                    },
                    Some(v) => {
                        // Allocate a location for the stream
                        let loc = match s.streams.get(&x) {
                            None => self.allocator.alloc(),
                            Some(loc) => *loc,
                        };

                        let mut s = s.clone();
                        let stream = v.replace_stream_loc(loc).unwrap();
                        s.extend(loc, stream.clone());

                        // Map the stream to the location
                        s.streams.insert(x.clone(), loc);
                        s.now.insert(loc, stream.clone());

                        Ok((stream, s))
                    }
                }
            }
            LetIn(var, e1, e2) => {
                let (val, _s) = self.eval(*e1, s)?;
                self.eval(e2.substitute(&var, &val).1, _s)
            }
        }
    }

    fn eval_bop(&mut self, e1: Expr, op: BOpcode, e2: Expr, s: Store) -> Result<(Expr, Store)> {
        let (_e1, _s) = self.eval(e1.clone(), s)?;
        let (_e2, __s) = self.eval(e2.clone(), _s)?;

        use BOpcode::*;
        use Expr::*;
        match (_e1, op, _e2) {
            (Int(v1), Mul, Int(v2)) => Ok((Int(v1 * v2), __s)),
            (Float(v1), Mul, Float(v2)) => Ok((Float(v1 * v2), __s)),
            (Int(v1), Div, Int(v2)) => Ok((Int(v1 / v2), __s)),
            (Float(v1), Div, Float(v2)) => Ok((Float(v1 / v2), __s)),
            (Int(v1), Add, Int(v2)) => Ok((Int(v1 + v2), __s)),
            (Float(v1), Add, Float(v2)) => Ok((Float(v1 + v2), __s)),
            (Int(v1), Sub, Int(v2)) => Ok((Int(v1 - v2), __s)),
            (Float(v1), Sub, Float(v2)) => Ok((Float(v1 - v2), __s)),
            (Int(v1), Mod, Int(v2)) => Ok((Int(v1 % v2), __s)),

            (v, Cons, List(mut list)) => {
                list.push_front(Box::new(v));
                Ok((List(list), __s))
            }

            (Int(v1), Eq, Int(v2)) => Ok((Bool(v1 == v2), __s)),
            (Float(v1), Eq, Float(v2)) => Ok((Bool(v1 == v2), __s)),
            (Int(v1), Lt, Int(v2)) => Ok((Bool(v1 < v2), __s)),
            (Float(v1), Lt, Float(v2)) => Ok((Bool(v1 < v2), __s)),
            (Int(v1), Gt, Int(v2)) => Ok((Bool(v1 > v2), __s)),
            (Float(v1), Gt, Float(v2)) => Ok((Bool(v1 > v2), __s)),

            (Bool(v1), And, Bool(v2)) => Ok((Bool(v1 && v2), __s)),
            (Bool(v1), Or, Bool(v2)) => Ok((Bool(v1 || v2), __s)),
            _ => Err(
                UncaughtTypeError(format!("{:?}", BinOp(Box::new(e1), op, Box::new(e2)))).into(),
            ),
        }
    }

    // Step an expression e, forward
    pub fn step(&mut self) -> Result<()> {
        let streams = self.streams.clone();
        for (stream, expr) in streams {
            match expr.is_stream() {
                Some((e, loc)) => {
                    // Take the current value and pass it to the outputs
                    self.stream_outs.insert(stream.clone(), e);
                    // Update the stream's term to the stream on the next time
                    self.streams
                        .insert(stream.clone(), Expr::Adv(Box::new(loc.clone())));
                    self.globals.insert(stream, Expr::Adv(Box::new(loc)));
                }
                None => return Err(ExpressionDoesNotStepError(format!("{:?}", expr)).into()),
            }
        }
        self.update_store();

        Ok(())
    }

    pub fn update_store(&mut self) {
        let mut to_dealloc: HashSet<u64> = self.store.now.keys().cloned().collect();
        self.store.now = self.store.later.clone();
        self.store.later = HashMap::new();

        use Expr::{Into, Location, Tuple};
        for (stream, loc) in &self.store.streams {
            self.store.now.insert(
                *loc,
                Into(Box::new(Tuple(vec![
                    Box::new(self.stream_outs.get(stream).unwrap().clone()),
                    Box::new(Location(*loc)),
                ]))),
            );
            self.store.later.insert(*loc, Expr::Unit);
            to_dealloc.remove(loc);
        }
        self.allocator.dealloc_set(to_dealloc);
    }

    fn match_pattern(val: &Expr, pattern: &Pattern) -> Result<(bool, Vec<(String, Expr)>)> {
        use Pattern::*;
        match pattern {
            Underscore => Ok((true, Vec::with_capacity(0))),
            Bool(b1) => match val {
                Expr::Bool(b2) => {
                    if b1 == b2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Int(i1) => match val {
                Expr::Int(i2) => {
                    if i1 == i2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Float(f1) => match val {
                Expr::Float(f2) => {
                    if f1 == f2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            String(s1) => match val {
                Expr::String(s2) => {
                    if s1 == s2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Unit => match val {
                Expr::Unit => Ok((true, Vec::with_capacity(0))),
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Tuple(patterns) => match val.clone() {
                Expr::Tuple(vals) => {
                    if patterns.len() == vals.len() {
                        let (mut matches, mut subs) = (true, Vec::new());
                        for (p, v) in patterns.into_iter().zip(vals) {
                            let (b, mut ss) = Self::match_pattern(&v, &p)?;

                            matches = matches && b;
                            subs.append(&mut ss);
                        }
                        Ok((matches, subs))
                    } else {
                        Err(
                            PatternMatchError(format!("{:?}", pattern), format!("{:?}", val))
                                .into(),
                        )
                    }
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            List(patterns) => match val.clone() {
                Expr::List(vals) => {
                    if patterns.len() == vals.len() {
                        let (mut matches, mut subs) = (true, Vec::new());
                        for (p, v) in patterns.into_iter().zip(vals) {
                            let (b, mut ss) = Self::match_pattern(&v, &p)?;

                            matches = matches && b;
                            subs.append(&mut ss);
                        }
                        Ok((matches, subs))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Variant(c1, o1) => match val {
                Expr::Variant(c2, o2) => {
                    if c1 != c2 {
                        return Ok((false, Vec::with_capacity(0)));
                    }
                    if o1.is_none() && o2.is_none() {
                        return Ok((true, Vec::with_capacity(0)));
                    }
                    if o1.is_some() && o2.is_some() {
                        let (p, v) = (*o1.clone().unwrap(), *o2.clone().unwrap());
                        return Self::match_pattern(&v, &p);
                    }
                    Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into())
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Struct(s1, patterns) => match val {
                Expr::Struct(s2, vals) => {
                    if s1 != s2 {
                        return Err(PatternMatchError(
                            format!("{:?}", pattern),
                            format!("{:?}", val),
                        )
                        .into());
                    }

                    let (mut matches, mut subs) = (true, Vec::new());
                    for (i, (field, o)) in patterns.into_iter().enumerate() {
                        if i >= vals.len() {
                            return Err(PatternMatchError(
                                format!("{:?}", pattern),
                                format!("{:?}", val),
                            )
                            .into());
                        }
                        if field == &"..".to_string() {
                            break;
                        }

                        if field != &vals[i].0 {
                            return Err(PatternMatchError(
                                format!("{:?}", pattern),
                                format!("{:?}", val),
                            )
                            .into());
                        }

                        let (b, mut ss) = match o {
                            None => Self::match_pattern(&vals[i].1, &Var(field.clone()))?,
                            Some(p) => Self::match_pattern(&vals[i].1, &p)?,
                        };

                        matches = matches && b;
                        subs.append(&mut ss);
                    }
                    Ok((matches, subs))
                }
                _ => Err(PatternMatchError(format!("{}", pattern), format!("{}", val)).into()),
            },
            Cons(p1, p2) => match val {
                Expr::List(vals) => {
                    let (b1, mut subs1) = Self::match_pattern(&vals[0], p1)?;
                    let tail = vals.range(1..).cloned().collect();
                    let (b2, mut subs2) = Self::match_pattern(&Expr::List(tail), p2)?;

                    subs1.append(&mut subs2);
                    Ok((b1 && b2, subs1))
                }
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Stream(p1, p2) => match val {
                Expr::Into(tuple) => match &**tuple {
                    Expr::Tuple(pair) => {
                        if pair.len() != 2 {
                            return Err(PatternMatchError(
                                format!("{:?}", pattern),
                                format!("{:?}", val),
                            )
                            .into());
                        }
                        if !matches!(&*pair[1], Expr::Location(_)) {
                            return Err(PatternMatchError(
                                format!("{:?}", pattern),
                                format!("{:?}", val),
                            )
                            .into());
                        }

                        let (b1, mut subs1) = Self::match_pattern(&pair[0], p1)?;
                        let (b2, mut subs2) = Self::match_pattern(&pair[1], p2)?;

                        subs1.append(&mut subs2);
                        Ok((b1 && b2, subs1))
                    }
                    _ => Err(
                        PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into(),
                    ),
                },
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Or(p1, p2) => {
                let (b, subs) = Self::match_pattern(val, p1)?;
                if b {
                    Ok((b, subs))
                } else {
                    Self::match_pattern(val, p2)
                }
            }
            Var(var) => Ok((true, vec![(var.clone(), val.clone())])),
        }
    }
}

impl Store {
    pub fn new() -> Self {
        Store {
            now: HashMap::new(),
            later: HashMap::new(),
            streams: HashMap::new(),
        }
    }

    fn extend(&mut self, loc: u64, term: Expr) {
        self.later.insert(loc, term);
    }

    fn now(&self) -> Self {
        Self {
            now: self.now.clone(),
            later: HashMap::new(),
            streams: self.streams.clone(),
        }
    }
}

impl Expr {
    fn is_stream(&self) -> Option<(Expr, Expr)> {
        use Expr::{Into, Location, Tuple};
        match self {
            Into(tuple) => match &**tuple {
                Tuple(pair) => {
                    if pair.len() != 2 {
                        return None;
                    }
                    if !matches!(&*pair[1], Location(_)) {
                        return None;
                    }
                    Some((*pair[0].clone(), *pair[1].clone()))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn replace_stream_loc(&self, loc: u64) -> Option<Expr> {
        use Expr::{Into, Location, Tuple};
        match self {
            Into(tuple) => match &**tuple {
                Tuple(pair) => {
                    if pair.len() != 2 {
                        return None;
                    }
                    if !matches!(&*pair[1], Location(_)) {
                        return None;
                    }
                    Some(Into(Box::new(Tuple(vec![
                        pair[0].clone(),
                        Box::new(Location(loc)),
                    ]))))
                }
                _ => None,
            },
            _ => None,
        }
    }
}
