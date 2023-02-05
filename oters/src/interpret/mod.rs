mod allocator;
mod errors;
mod tests;

use self::allocator::Allocator;
use self::errors::InterpretError::*;
use crate::export::{PathExportFns, Value};
use crate::exprs::{BOpcode, Expr, LetBinding, UOpcode};
use crate::parser::ast::Pattern;

use daggy::petgraph::visit::{EdgeRef, IntoEdges};
use daggy::{Dag, NodeIndex, Walker};
use std::collections::{HashMap, HashSet, VecDeque};
use std::vec;

use anyhow::Result;

pub struct Interpreter {
    allocator: Allocator,
    globals: HashMap<(Vec<String>, String), Expr>,
    eval_order: Vec<(Vec<String>, String)>,
    imports: PathExportFns,
    store: Store,
    mut_rec_streams: HashMap<(Vec<String>, String), Expr>,
    current_path: Vec<String>,
}

// Potentially change locations to be shared references
#[derive(Clone, Debug)]
pub struct Store {
    pub(super) now: HashMap<u64, Expr>,
    pub(super) later: HashMap<u64, Expr>,
}

impl Interpreter {
    pub fn new(
        bindings: Dag<Vec<LetBinding>, String>,
        imports: PathExportFns,
        files: Vec<String>,
    ) -> Result<Self> {
        let mut interp = Interpreter {
            allocator: Allocator::new(),
            globals: HashMap::new(),
            eval_order: Vec::new(),
            imports,
            store: Store::new(),
            mut_rec_streams: HashMap::new(),
            current_path: Vec::new(),
        };
        interp.init(bindings, files)?;

        Ok(interp)
    }

    fn init(&mut self, bindings: Dag<Vec<LetBinding>, String>, files: Vec<String>) -> Result<()> {
        let (mut std, mut gui, mut user_code) = (Vec::new(), Vec::new(), Vec::new());
        for edge in bindings.edges(0.into()) {
            if edge.weight() == "std" {
                std = flatten(&bindings, &vec!["std".to_string()], edge.target());
            } else if edge.weight() == "gui" {
                gui = flatten(&bindings, &vec!["gui".to_string()], edge.target());
            } else {
                // Insert user code files in order of dependency
                user_code.insert(
                    files.iter().enumerate().fold(0, |ret, (i, s)| {
                        let ret = if s == edge.weight() { i } else { ret };
                        ret
                    }),
                    (vec![edge.weight().clone()], bindings[edge.target()].clone()),
                )
            }
        }
        for (path, bindings) in std {
            self.init_bindings(bindings, path)?;
        }
        for (path, bindings) in gui {
            self.init_bindings(bindings, path)?;
        }
        for (path, bindings) in user_code {
            self.init_bindings(bindings, path)?;
        }

        Ok(())
    }

    pub fn init_bindings(&mut self, bindings: Vec<LetBinding>, path: Vec<String>) -> Result<()> {
        self.current_path = path.clone();
        for binding in bindings {
            match binding {
                LetBinding::Let(pat, expr) => {
                    // Reduce all the expressions
                    let (e, s) = self.eval(expr.clone(), self.store.clone())?;

                    let (res, bound_vars) = Self::match_pattern(&e, &pat)?;
                    if !res {
                        return Err(PatternMatchError(pat, expr).into());
                    }

                    for (var, e) in bound_vars {
                        self.globals.insert((path.clone(), var.clone()), e);
                        self.eval_order.push((path.clone(), var));
                    }
                    self.store = s;
                }

                LetBinding::LetAndWith(x, e1, y, e2, e3) => {
                    let (v, mut s) = self.eval(e3.clone(), self.store.clone())?;

                    let loc_2 = self.allocator.alloc();
                    let y_str = Expr::Into(Box::new(Expr::Tuple(vec![
                        Box::new(v),
                        Box::new(Expr::Location(loc_2)),
                    ])));
                    s.extend(loc_2, y_str.clone());
                    self.globals
                        .insert((path.clone(), y.clone()), y_str.clone());

                    let (v1, s) = self.eval(e1.clone(), s)?;
                    self.eval_order.push((path.clone(), x.clone()));
                    self.globals.insert((path.clone(), x), v1);

                    let (v2, s) = self.eval(e2.clone(), s)?;
                    self.eval_order.push((path.clone(), y.clone()));
                    self.globals.insert((path.clone(), y.clone()), v2.clone());
                    self.mut_rec_streams.insert((path.clone(), y), v2);

                    self.store = s;
                }
                LetBinding::Use(use_path, val) => {
                    if val == "*" {
                        for ((path_2, val), v) in self.globals.clone() {
                            if use_path == path_2 {
                                self.globals.insert((path.clone(), val.clone()), v.clone());
                            }
                        }
                        for ((path_2, val), v) in self.imports.clone() {
                            if use_path == path_2 {
                                self.imports.insert((path.clone(), val.clone()), v.clone());
                            }
                        }
                    } else {
                        let v = self.globals.get(&(use_path.clone(), val.clone())).cloned();
                        match v {
                            Some(v) => {
                                self.globals.insert((path.clone(), val), v.clone());
                            }
                            None => {
                                let v = self
                                    .imports
                                    .get(&(use_path.clone(), val.clone()))
                                    .cloned()
                                    .unwrap();
                                self.imports.insert((path.clone(), val), v.clone());
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn eval_step(&mut self) -> Result<()> {
        self.step();

        for (path, var) in self.eval_order.clone() {
            // println!("{}: {}", stream, self.stream_outs.get(&stream).unwrap());
            // println!("{}: {}", var, self.globals.get(&var).unwrap());
            // println!("{:?}\n", self.store.now.keys());
            self.current_path = path.clone();
            let e = self
                .globals
                .get(&(path.clone(), var.clone()))
                .unwrap()
                .clone();

            let (e, s) = self.eval(e, self.store.clone())?;
            self.store = s;

            self.globals.insert((path, var), e);
        }
        Ok(())
    }

    pub fn eval(&mut self, e: Expr, s: Store) -> Result<(Expr, Store)> {
        use Expr::*;
        match e.clone() {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) | Location(_) => Ok((e, s)),
            BinOp(e1, op, e2) => {
                let (e_, s) = self.eval_bop(*e1, op, *e2, s)?;
                Ok((e_, s))
            }
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
                            },
                        ),
                    },
                    _ => Err(UncaughtTypeError(format!("{:?}", e)).into()),
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
            Struct(path, str, fields) => {
                let mut field_vec = Vec::new();
                let mut store = s.clone();

                for (f, e) in fields {
                    let (_e, _s) = self.eval(*e, store.clone())?;
                    field_vec.push((f, Box::new(_e)));
                    store = _s;
                }

                Ok((Struct(path, str, field_vec), store))
            }
            Variant(path, constr, opt) => match opt {
                None => Ok((Variant(path, constr, opt), s)),
                Some(e) => {
                    let (val, _s) = self.eval(*e.clone(), s)?;
                    Ok((Variant(path, constr, Some(Box::new(val))), _s))
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
                    Var(path, var) => {
                        let path = if path.is_empty() {
                            self.current_path.clone()
                        } else {
                            path.clone()
                        };

                        // If the function has been imported, call it with converted arguments
                        if !self.globals.contains_key(&(path.clone(), var.clone()))
                            && self.imports.contains_key(&(path.clone(), var.clone()))
                        {
                            let (func, arg_ts, _) =
                                self.imports.get(&(path, var.clone())).unwrap().clone();
                            let (val, _s) = self.eval(*e2, s)?;
                            let ret_val = func(Value::expr_to_args(val, arg_ts.len())?);
                            return Ok((ret_val.to_expr(), _s));
                        }
                    }
                    _ => (),
                }

                let (_e1, _s) = self.eval(*e1.clone(), s)?;
                match _e1 {
                    Fn(pat, expr) => {
                        let (val, __s) = self.eval(*e2.clone(), _s)?;
                        let (is_match, bindings) = Self::match_pattern(&val, &pat)?;
                        if !is_match {
                            return Err(PatternMatchError(pat, *e2).into());
                        }
                        let mut expr = *expr.clone();
                        for (var, val) in bindings {
                            expr = expr.substitute(&var, &val).1;
                        }
                        self.eval(expr, __s)
                    }
                    _ => Err(UncaughtTypeError(format!("{:?}", e1)).into()),
                }
            }
            ProjStruct(e, field) => {
                let (_e, _s) = self.eval(*e.clone(), s)?;
                match _e {
                    Struct(_, _, fields) => {
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
            Var(path, x) => {
                let path = if path.is_empty() {
                    self.current_path.clone()
                } else {
                    path
                };
                match self.globals.get(&(path.clone(), x.clone())) {
                    None => Err(UnboundVariableError(x).into()),
                    Some(v) => {
                        if let Some(stream) = self.mut_rec_streams.get(&(path, x)) {
                            Ok((stream.clone(), s))
                        } else {
                            Ok((v.clone(), s))
                        }
                    }
                }
            }
            LetIn(pat, e1, e2) => {
                let (val, _s) = self.eval(*e1.clone(), s)?;
                let (res, bound_vars) = Self::match_pattern(&val, &pat)?;
                if !res {
                    return Err(PatternMatchError(pat, *e1).into());
                }

                match &val {
                    Fn(arg, body) => {
                        // Substitution for recursive function according to Part 1B Semantics
                        if bound_vars.len() == 1 && val.is_static_recursive(&bound_vars[0].0) {
                            let var = bound_vars[0].0.clone();
                            return self.eval(
                                e2.substitute(
                                    &var,
                                    &Fn(
                                        arg.clone(),
                                        Box::new(LetIn(
                                            Pattern::Var(var.clone(), false),
                                            Box::new(val.clone()),
                                            body.clone(),
                                        )),
                                    ),
                                )
                                .1,
                                _s,
                            );
                        }
                    }
                    _ => (),
                }

                let mut e2 = *e2;
                for (var, val) in bound_vars {
                    e2 = e2.substitute(&var, &val).1;
                }
                self.eval(e2, _s)
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
    pub fn step(&mut self) {
        for (var, stream) in self.mut_rec_streams.iter_mut() {
            *stream = self.globals.get(var).unwrap().clone();
        }

        let globals = self.globals.clone();
        for (var, expr) in globals {
            self.globals.insert(var, expr.step_value());
        }
        self.update_store();
    }

    pub fn update_store(&mut self) {
        let to_dealloc: HashSet<u64> = self.store.now.keys().cloned().collect();
        self.store.now = self.store.later.clone();
        self.store.later = HashMap::new();

        self.allocator.dealloc_set(to_dealloc);
    }

    // Returns if there has been a match and any variable bindings
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
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Int(i1) => match val {
                Expr::Int(i2) => {
                    if i1 == i2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Float(f1) => match val {
                Expr::Float(f2) => {
                    if f1 == f2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            String(s1) => match val {
                Expr::String(s2) => {
                    if s1 == s2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Unit => match val {
                Expr::Unit => Ok((true, Vec::with_capacity(0))),
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
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
                        Err(PatternMatchError(pattern.clone(), val.clone()).into())
                    }
                }
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
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
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Variant(_, c1, o1) => match val {
                Expr::Variant(_, c2, o2) => {
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
                    Err(PatternMatchError(pattern.clone(), val.clone()).into())
                }
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Struct(path1, s1, patterns) => match val {
                Expr::Struct(path2, s2, vals) => {
                    if path1 != path2 {
                        return Err(PatternMatchError(pattern.clone(), val.clone()).into());
                    }
                    if s1 != s2 {
                        return Err(PatternMatchError(pattern.clone(), val.clone()).into());
                    }

                    let (mut matches, mut subs) = (true, Vec::new());
                    for (i, (field, o)) in patterns.into_iter().enumerate() {
                        if i >= vals.len() {
                            return Err(PatternMatchError(pattern.clone(), val.clone()).into());
                        }
                        if field == ".." {
                            break;
                        }

                        if field != &vals[i].0 {
                            return Err(PatternMatchError(pattern.clone(), val.clone()).into());
                        }

                        let (b, mut ss) = match o {
                            None => Self::match_pattern(&vals[i].1, &Var(field.clone(), false))?,
                            Some(p) => Self::match_pattern(&vals[i].1, &p)?,
                        };

                        matches = matches && b;
                        subs.append(&mut ss);
                    }
                    Ok((matches, subs))
                }
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Cons(p1, p2) => match val {
                Expr::List(vals) => {
                    let (b1, mut subs1) = Self::match_pattern(&vals[0], p1)?;
                    let tail = vals.range(1..).cloned().collect();
                    let (b2, mut subs2) = Self::match_pattern(&Expr::List(tail), p2)?;

                    subs1.append(&mut subs2);
                    Ok((b1 && b2, subs1))
                }
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Stream(p1, p2) => match val {
                Expr::Into(tuple) => match &**tuple {
                    Expr::Tuple(pair) => {
                        if pair.len() != 2 {
                            return Err(PatternMatchError(pattern.clone(), val.clone()).into());
                        }
                        if !matches!(&*pair[1], Expr::Location(_)) {
                            return Err(PatternMatchError(pattern.clone(), val.clone()).into());
                        }

                        let (b1, mut subs1) = Self::match_pattern(&pair[0], p1)?;
                        let (b2, mut subs2) = Self::match_pattern(&pair[1], p2)?;

                        subs1.append(&mut subs2);
                        Ok((b1 && b2, subs1))
                    }
                    _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
                },
                _ => Err(PatternMatchError(pattern.clone(), val.clone()).into()),
            },
            Or(p1, p2) => {
                let (b, subs) = Self::match_pattern(val, p1)?;
                if b {
                    Ok((b, subs))
                } else {
                    Self::match_pattern(val, p2)
                }
            }
            Var(var, _) => Ok((true, vec![(var.clone(), val.clone())])),
        }
    }
}

impl Store {
    pub fn new() -> Self {
        Store {
            now: HashMap::new(),
            later: HashMap::new(),
        }
    }

    fn extend(&mut self, loc: u64, term: Expr) {
        self.later.insert(loc, term);
    }

    fn now(&self) -> Self {
        Self {
            now: self.now.clone(),
            later: HashMap::new(),
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

    fn step_value(&self) -> Expr {
        use Expr::*;
        match self {
            Bool(_) | Int(_) | Float(_) | String(_) | Unit | Var(..) | Location(_) | Fn(..)
            | Fix(..) => self.clone(),
            Stable(v) => Stable(Box::new(v.step_value())),
            Into(v) => match *v.clone() {
                Tuple(pair) => {
                    if pair.len() == 2 {
                        if let Location(loc) = *pair[1] {
                            Adv(Box::new(Location(loc)))
                        } else {
                            Into(Box::new(v.step_value()))
                        }
                    } else {
                        Into(Box::new(v.step_value()))
                    }
                }
                _ => Into(Box::new(v.step_value())),
            },
            List(list) => List(list.iter().map(|v| Box::new(v.step_value())).collect()),
            Tuple(tuple) => Tuple(tuple.iter().map(|v| Box::new(v.step_value())).collect()),
            Struct(path, name, fields) => Struct(
                path.clone(),
                name.clone(),
                fields
                    .iter()
                    .map(|(f, v)| (f.clone(), Box::new(v.step_value())))
                    .collect(),
            ),
            Variant(path, name, v) => Variant(
                path.clone(),
                name.clone(),
                v.clone().map(|v| Box::new(v.step_value())),
            ),
            _ => self.clone(),
        }
    }
}

fn flatten(
    dag: &Dag<Vec<LetBinding>, String>,
    path: &Vec<String>,
    root: NodeIndex,
) -> Vec<(Vec<String>, Vec<LetBinding>)> {
    let mut children = dag.children(root);
    let mut bindings = vec![(path.clone(), dag[root].clone())];

    while let Some((e_id, n_id)) = children.walk_next(dag) {
        let mut child_path = path.clone();
        child_path.push(dag.edge_weight(e_id).unwrap().clone());

        let mut child_bindings = flatten(dag, &child_path, n_id);
        bindings.append(&mut child_bindings);
    }
    bindings
}
