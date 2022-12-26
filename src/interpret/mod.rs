mod errors;

use std::collections::HashMap;
use std::vec;

use self::errors::InterpretError::*;
use crate::exprs::{BOpcode, Expr, UOpcode};
use crate::parser::ast::Pattern;

use anyhow::{Ok, Result};

use petgraph::graph::DiGraph;

pub struct Interpreter {
    new_loc: u64,
    current_deps: Vec<String>,
    flow_graph: DiGraph<String, ()>,
    globals: HashMap<String, Expr>,
    streams: HashMap<String, Expr>,
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
    pub fn new() -> Self {
        Interpreter {
            new_loc: 0,
            current_deps: Vec::new(),
            flow_graph: DiGraph::new(),
            globals: HashMap::new(),
            streams: HashMap::new(),
            store: Store::new(),
            stream_outs: HashMap::new(),
        }
    }

    pub fn init(&mut self, exprs: Vec<(String, Expr)>) -> Result<()> {
        for (id, expr) in exprs {
            self.current_deps = Vec::new();
            // Reduce all the expressions
            let (e, s) = self.eval(expr, self.store.clone())?;

            // If the resulting value is a stream then add it to the stream flow graph
            if !matches!(e.is_stream(), None) {
                let current_index = self.flow_graph.add_node(id.clone());
                // Any streams used in the expression are added to current_deps
                for dep in &self.current_deps {
                    let dep_index = self
                        .flow_graph
                        .node_indices()
                        .find(|i| &self.flow_graph[*i] == dep)
                        .unwrap();
                    // Add an edges from the dependency to the current stream
                    self.flow_graph.update_edge(dep_index, current_index, ());
                }
                self.streams.insert(id.clone(), e.clone());
            }
            // Globals contain reduced expressions e.g. functions, values, streams, etc.
            self.globals.insert(id.clone(), e);
            self.store = s;
        }

        Ok(())
    }

    pub fn eval_loop(&mut self) -> Result<()> {
        if self.streams.len() == 0 {
            return Ok(());
        }

        // Warning: Breaks with cycles
        // Topological sort makes sure dependencies are evaluated before dependents
        let streams = petgraph::algo::toposort(
            &self.flow_graph,
            Some(&mut petgraph::algo::DfsSpace::new(&self.flow_graph)),
        )
        .unwrap();

        loop {
            self.step()?;
            for index in &streams {
                let stream = self.flow_graph[*index].clone();
                let e = self.streams.get(&stream).unwrap().clone();
                println!("{}: {}", stream, self.stream_outs.get(&stream).unwrap());

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

        Ok(())
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
                let loc = self.alloc();
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
                let mut list_vec = Vec::new();
                let mut store = s.clone();

                for e in list {
                    let (_e, _s) = self.eval(*e.clone(), store.clone())?;
                    list_vec.push(Box::new(_e));
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
            Seq(e1, e2) => match *e1 {
                Let(var, expr) => {
                    let (val, _s) = self.eval(*expr, s)?;
                    self.eval(e2.substitute(&var, &val).1, _s)
                }
                e1 => {
                    let (_e1, _s) = self.eval(e1.clone(), s)?;
                    match _e1 {
                        Unit => self.eval(*e2, _s),
                        _ => Err(UncaughtTypeError(format!("{:?}", e1)).into()),
                    }
                }
            },
            App(e1, e2) => {
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
                let expr = self.globals.get(&x).map(|e| e.clone());
                match expr {
                    None => Err(UnboundVariableError(x).into()),
                    Some(v) => {
                        if !self.streams.contains_key(&x) {
                            return Ok((v.clone(), s));
                        }

                        // If variable is another stream add it to this stream's dependencies
                        self.current_deps.push(x.clone());

                        // Allocate a location for the stream
                        let loc = self.alloc();
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
            Let(..) => unreachable!("Let expression found outside block"),
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

            // O(n) but should be O(1)
            (v, Cons, List(list)) => {
                let mut list = list.clone();
                list.insert(0, Box::new(v));
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
                    self.streams.insert(stream, Expr::Adv(Box::new(loc)));
                }
                None => return Err(ExpressionDoesNotStepError(format!("{:?}", expr)).into()),
            }
        }
        self.update_store();

        Ok(())
    }

    pub fn update_store(&mut self) {
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
        }
    }

    fn alloc(&mut self) -> u64 {
        let loc = self.new_loc;
        self.new_loc += 1;
        loc
    }

    fn match_pattern(val: &Expr, pattern: &Pattern) -> Result<(bool, Vec<(String, Expr)>)> {
        use Pattern::*;
        match pattern {
            Underscore => Ok((true, Vec::with_capacity(0))),
            True => {
                if matches!(val, Expr::Bool(true)) {
                    Ok((true, Vec::with_capacity(0)))
                } else if matches!(val, Expr::Bool(false)) {
                    Ok((false, Vec::with_capacity(0)))
                } else {
                    Err(PatternMatchError(format!("{:?}", True), format!("{:?}", val)).into())
                }
            }
            False => {
                if matches!(val, Expr::Bool(false)) {
                    Ok((true, Vec::with_capacity(0)))
                } else if matches!(val, Expr::Bool(true)) {
                    Ok((false, Vec::with_capacity(0)))
                } else {
                    Err(PatternMatchError(format!("{:?}", True), format!("{:?}", val)).into())
                }
            }
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
                        return Err(PatternMatchError(
                            format!("{:?}", pattern),
                            format!("{:?}", val),
                        )
                        .into());
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
                _ => Err(PatternMatchError(format!("{:?}", pattern), format!("{:?}", val)).into()),
            },
            Cons(p1, p2) => match val {
                Expr::List(vals) => {
                    let (b1, mut subs1) = Self::match_pattern(&vals[0], p1)?;
                    let (b2, mut subs2) = Self::match_pattern(&Expr::List(vals[1..].into()), p2)?;

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
