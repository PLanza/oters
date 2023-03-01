mod allocator;
mod errors;
mod tests;

use self::allocator::Allocator;
use self::errors::InterpretError::*;
use crate::errors::SpError;
use crate::export::{PathExportFns, Value};
use crate::exprs::{BOpcode, Expr, LetBinding, SpExpr, UOpcode};
use crate::parser::ast::Pattern;
use crate::parser::span::Spanned;

use daggy::petgraph::visit::{EdgeRef, IntoEdges};
use daggy::{Dag, NodeIndex, Walker};
use std::collections::{HashMap, HashSet, VecDeque};
use std::vec;

use anyhow::Result;

pub struct Interpreter {
    allocator: Allocator,
    globals: HashMap<(Vec<String>, String), SpExpr>,
    eval_order: Vec<(Vec<String>, String, u64)>,
    imports: PathExportFns,
    store: Store,
    mut_rec_streams: HashSet<(Vec<String>, String)>,
    mut_rec_prevs: HashMap<u64, SpExpr>,
    current_path: Vec<String>,
}

// Potentially change locations to be shared references
#[derive(Clone, Debug)]
pub struct Store {
    pub(super) now: HashMap<u64, SpExpr>,
    pub(super) later: HashMap<u64, SpExpr>,
}

impl Interpreter {
    pub fn new(
        bindings: Dag<Vec<LetBinding>, String>,
        imports: PathExportFns,
        files: Vec<String>,
    ) -> Result<Self, SpError> {
        let mut interp = Interpreter {
            allocator: Allocator::new(),
            globals: HashMap::new(),
            eval_order: Vec::new(),
            imports,
            store: Store::new(),
            mut_rec_streams: HashSet::new(),
            mut_rec_prevs: HashMap::new(),
            current_path: Vec::new(),
        };
        interp.init(bindings, files)?;

        Ok(interp)
    }

    fn init(
        &mut self,
        bindings: Dag<Vec<LetBinding>, String>,
        files: Vec<String>,
    ) -> Result<(), SpError> {
        let (mut std, mut gui, mut user_code) = (
            Vec::new(),
            Vec::new(),
            files
                .iter()
                .map(|_| (Vec::new(), Vec::new()))
                .collect::<Vec<(Vec<String>, Vec<LetBinding>)>>(),
        );
        for edge in bindings.edges(0.into()) {
            if edge.weight() == "std" {
                std = flatten(&bindings, &vec!["std".to_string()], edge.target());
            } else if edge.weight() == "gui" {
                gui = flatten(&bindings, &vec!["gui".to_string()], edge.target());
            } else {
                // Insert user code files in order of dependency
                user_code[files.iter().enumerate().fold(0, |ret, (i, s)| {
                    let ret = if s == edge.weight() { i } else { ret };
                    ret
                })] = (vec![edge.weight().clone()], bindings[edge.target()].clone());
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

    pub fn init_bindings(
        &mut self,
        bindings: Vec<LetBinding>,
        path: Vec<String>,
    ) -> Result<(), SpError> {
        self.current_path = path.clone();
        for binding in bindings {
            match binding {
                LetBinding::Let(pat, expr) => {
                    // Reduce all the expressions
                    let (e, s) = self.eval(expr.clone(), self.store.clone())?;

                    let (res, bound_vars) = Self::match_pattern(&e, &pat)?;
                    if !res {
                        return Err(SpError::new(
                            PatternMatchError(*pat.term, *expr.term).into(),
                            (pat.span.0, expr.span.1),
                        ));
                    }

                    for (var, e) in bound_vars {
                        if let Some(loc) = e.is_stream() {
                            self.eval_order.push((path.clone(), var.clone(), loc));
                        }
                        self.globals.insert((path.clone(), var), e);
                    }
                    self.store = s;
                }

                LetBinding::LetAndWith(pat1, e1, y, e2, e3) => {
                    let (v, mut s) = self.eval(e3.clone(), self.store.clone())?;

                    let loc_2 = self.allocator.alloc();
                    let y_str = SpExpr::new(
                        Expr::Into(SpExpr::new(
                            Expr::Tuple(vec![v, SpExpr::new(Expr::Location(loc_2), e2.span)]),
                            e2.span,
                        )),
                        e2.span,
                    );
                    s.extend(loc_2, y_str.clone());
                    self.eval_order.push((path.clone(), y.clone(), loc_2));
                    self.globals
                        .insert((path.clone(), y.clone()), y_str.clone());

                    let (v1, s) = self.eval(e1.clone(), s)?;
                    let (res, bound_v1s) = Self::match_pattern(&v1, &pat1)?;
                    if !res {
                        return Err(SpError::new(
                            PatternMatchError(*pat1.term, *e1.term).into(),
                            (pat1.span.0, e1.span.1),
                        ));
                    }
                    for (var, e) in bound_v1s {
                        if let Some(loc) = e.is_stream() {
                            self.eval_order.push((path.clone(), var.clone(), loc));
                            self.mut_rec_streams.insert((path.clone(), var.clone()));
                        }
                        self.globals.insert((path.clone(), var), e);
                    }

                    let (v2, s) = self.eval(e2.clone(), s)?;
                    self.globals.insert((path.clone(), y.clone()), v2.clone());

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

    pub fn eval_step(&mut self) -> Result<(), SpError> {
        self.step();

        for (path, var, loc) in self.eval_order.clone() {
            self.current_path = path.clone();
            let e = self
                .globals
                .get(&(path.clone(), var.clone()))
                .unwrap()
                .clone();
            if self.mut_rec_prevs.contains_key(&loc) {
                self.mut_rec_prevs.remove(&loc);
            }

            let (e, s) = self.eval(e, self.store.clone())?;
            let (e, temp_loc) = e.replace_stream_loc(loc).unwrap();
            let store_val = s.later.get(&temp_loc).cloned().unwrap();

            self.store = s;
            self.store.now.insert(loc, e.clone());
            self.store.later.insert(loc, store_val);

            self.globals.insert((path, var), e);
        }
        Ok(())
    }

    pub fn eval(&mut self, e: SpExpr, s: Store) -> Result<(SpExpr, Store), SpError> {
        let span = e.span;
        let e_term = *e.term.clone();
        use Expr::*;
        match *e.term {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) | Location(_) => Ok((e, s)),
            BinOp(e1, op, e2) => {
                let (e_, s) = self.eval_bop(e1, op, e2, s)?;
                Ok((e_, s))
            }
            UnOp(UOpcode::Neg, e) => {
                let (_e, _s) = self.eval(e.clone(), s)?;
                match _e.term.as_ref() {
                    Int(v) => Ok((SpExpr::new(Int(-v), span), _s)),
                    Float(v) => Ok((SpExpr::new(Float(-v), span), _s)),
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            UnOp(UOpcode::Not, e) => {
                let (_e, _s) = self.eval(e.clone(), s)?;
                match _e.term.as_ref() {
                    Bool(v) => Ok((SpExpr::new(Bool(!v), span), _s)),
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            Delay(e) => {
                let loc = self.allocator.alloc();
                let mut s = s.clone();
                s.extend(loc, e);
                Ok((SpExpr::new(Location(loc), span), s))
            }
            Stable(e) => Ok((SpExpr::new(Stable(e), span), s)),
            Adv(e) => {
                let s_n = s.now();
                let (_e, _s_n) = self.eval(e.clone(), s_n)?;

                match _e.term.as_ref() {
                    Location(l) => match self.mut_rec_prevs.get(l).cloned() {
                        None => match _s_n.now.get(&l) {
                            Some(term) => self.eval(
                                term.clone(),
                                Store {
                                    now: _s_n.now,
                                    later: s.later,
                                },
                            ),
                            None => Err(SpError::new(UnboundLocationError(e_term).into(), span)),
                        },
                        Some(term) => self.eval(
                            term.clone(),
                            Store {
                                now: _s_n.now,
                                later: s.later,
                            },
                        ),
                    },
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            Unbox(e) => {
                let (_e, _s) = self.eval(e.clone(), s)?;
                match _e.term.as_ref() {
                    Stable(term) => self.eval(term.clone(), _s),
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            Out(e) => {
                let (_e, _s) = self.eval(e.clone(), s)?;
                match _e.term.as_ref() {
                    Into(value) => Ok((value.clone(), _s)),
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            Into(e) => {
                let (value, _s) = self.eval(e.clone(), s)?;
                Ok((SpExpr::new(Into(value), span), _s))
            }
            List(list) => {
                let mut list_vec = VecDeque::new();
                let mut store = s.clone();

                for e in list {
                    let (_e, _s) = self.eval(e.clone(), store.clone())?;
                    list_vec.push_back(_e);
                    store = _s;
                }

                Ok((SpExpr::new(List(list_vec), span), store))
            }
            Tuple(tuple) => {
                let mut tuple_vec = Vec::new();
                let mut store = s.clone();

                for e in tuple {
                    let (_e, _s) = self.eval(e, store.clone())?;
                    tuple_vec.push(_e);
                    store = _s;
                }

                Ok((SpExpr::new(Tuple(tuple_vec), span), store))
            }
            Struct(path, str, fields) => {
                let mut field_vec = Vec::new();
                let mut store = s.clone();

                for (f, e) in fields {
                    let (_e, _s) = self.eval(e, store.clone())?;
                    field_vec.push((f, _e));
                    store = _s;
                }

                Ok((SpExpr::new(Struct(path, str, field_vec), span), store))
            }
            Variant(path, constr, opt) => match opt {
                None => Ok((SpExpr::new(Variant(path, constr, opt), span), s)),
                Some(e) => {
                    let (val, _s) = self.eval(e.clone(), s)?;
                    Ok((SpExpr::new(Variant(path, constr, Some(val)), span), _s))
                }
            },
            Fn(..) => Ok((e, s)),
            Fix(var, expr) => self.eval(
                expr.clone()
                    .substitute(
                        &var.clone(),
                        &SpExpr::new(
                            Stable(SpExpr::new(Delay(SpExpr::new(Fix(var, expr), span)), span)),
                            span,
                        ),
                    )
                    .1,
                s,
            ),
            If(e1, e2, e3) => {
                let (_e1, _s) = self.eval(e1.clone(), s)?;
                match _e1.term.as_ref() {
                    Bool(true) => self.eval(e2, _s),
                    Bool(false) => self.eval(e3, _s),
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            Seq(e1, e2) => {
                let (_e1, _s) = self.eval(e1.clone(), s)?;
                match _e1.term.as_ref() {
                    Unit => self.eval(e2, _s),
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            App(e1, e2) => {
                match e1.term.as_ref() {
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
                            let (val, _s) = self.eval(e2.clone(), s)?;
                            let ret_val = func(
                                Value::expr_to_args(val, arg_ts.len())
                                    .map_err(|e| SpError::new(e, e2.span))?,
                            );
                            return Ok((SpExpr::new(ret_val.to_expr(), span), _s));
                        }
                    }
                    _ => (),
                }

                let (_e1, _s) = self.eval(e1.clone(), s)?;
                match _e1.term.as_ref() {
                    Fn(pat, expr) => {
                        let (val, __s) = self.eval(e2.clone(), _s)?;
                        let (is_match, bindings) = Self::match_pattern(&val, &pat)?;
                        if !is_match {
                            return Err(SpError::new(
                                PatternMatchError(*pat.term.clone(), *e2.term.clone()).into(),
                                span,
                            ));
                        }
                        let mut expr = expr.clone();
                        for (var, val) in bindings {
                            expr = expr.substitute(&var, &val).1;
                        }
                        self.eval(expr, __s)
                    }
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            ProjStruct(e, field) => {
                let (_e, _s) = self.eval(e.clone(), s)?;
                match _e.term.as_ref() {
                    Struct(_, _, fields) => {
                        for (f, val) in fields {
                            if f.clone() == field {
                                return Ok((val.clone(), _s));
                            }
                        }
                        Err(SpError::new(
                            StructFieldMissingError(field, e_term).into(),
                            span,
                        ))
                    }
                    _ => Err(SpError::new(UncaughtTypeError(e_term).into(), span)),
                }
            }
            Match(e, patterns) => {
                let (val, _s) = self.eval(e.clone(), s)?;
                for (p, e_) in patterns {
                    let (matches, subs) = Self::match_pattern(&val, &p)?;
                    if !matches {
                        continue;
                    }

                    let mut e_ = e_.clone();
                    for (var, term) in subs {
                        e_ = e_.substitute(&var, &term).1;
                    }
                    return self.eval(e_, _s);
                }
                Err(SpError::new(NoPatternMatchesError(e_term).into(), span))
            }
            Var(path, x) => {
                let path = if path.is_empty() {
                    self.current_path.clone()
                } else {
                    path
                };
                match self.globals.get(&(path.clone(), x.clone())) {
                    None => Err(SpError::new(UnboundVariableError(x).into(), span)),
                    Some(v) => Ok((v.clone(), s)),
                }
            }
            LetIn(pat, e1, e2) => {
                let (val, _s) = self.eval(e1.clone(), s)?;
                let (res, bound_vars) = Self::match_pattern(&val, &pat)?;
                if !res {
                    return Err(SpError::new(
                        PatternMatchError(*pat.term, *e1.term).into(),
                        span,
                    ));
                }

                match val.term.as_ref() {
                    Fn(arg, body) => {
                        // Substitution for recursive function according to Part 1B Semantics
                        if bound_vars.len() == 1 && val.is_static_recursive(&bound_vars[0].0) {
                            let var = bound_vars[0].0.clone();
                            return self.eval(
                                e2.substitute(
                                    &var,
                                    &SpExpr::new(
                                        Fn(
                                            arg.clone(),
                                            SpExpr::new(
                                                LetIn(
                                                    Spanned {
                                                        term: Box::new(Pattern::Var(
                                                            var.clone(),
                                                            false,
                                                        )),
                                                        span: pat.span,
                                                    },
                                                    val.clone(),
                                                    body.clone(),
                                                ),
                                                e1.span,
                                            ),
                                        ),
                                        e1.span,
                                    ),
                                )
                                .1,
                                _s,
                            );
                        }
                    }
                    _ => (),
                }

                let mut e2 = e2;
                for (var, val) in bound_vars {
                    e2 = e2.substitute(&var, &val).1;
                }
                self.eval(e2, _s)
            }
        }
    }

    fn eval_bop(
        &mut self,
        e1: SpExpr,
        op: BOpcode,
        e2: SpExpr,
        s: Store,
    ) -> Result<(SpExpr, Store), SpError> {
        let (_e1, _s) = self.eval(e1.clone(), s)?;
        let (_e2, __s) = self.eval(e2.clone(), _s)?;

        use BOpcode::*;
        use Expr::*;
        let result = match (*_e1.term, op, *_e2.term) {
            (Int(v1), Mul, Int(v2)) => Ok((Int(v1 * v2), __s)),
            (Float(v1), Mul, Float(v2)) => Ok((Float(v1 * v2), __s)),
            (Int(v1), Div, Int(v2)) => Ok((Int(v1 / v2), __s)),
            (Float(v1), Div, Float(v2)) => Ok((Float(v1 / v2), __s)),
            (Int(v1), Add, Int(v2)) => Ok((Int(v1 + v2), __s)),
            (Float(v1), Add, Float(v2)) => Ok((Float(v1 + v2), __s)),
            (Int(v1), Sub, Int(v2)) => Ok((Int(v1 - v2), __s)),
            (Float(v1), Sub, Float(v2)) => Ok((Float(v1 - v2), __s)),
            (Int(v1), Mod, Int(v2)) => Ok((Int((v1 + v2) % v2), __s)),

            (v, Cons, List(mut list)) => {
                list.push_front(SpExpr::new(v, e1.span));
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

            _ => Err(SpError::new(
                UncaughtTypeError(BinOp(e1.clone(), op, e2.clone())).into(),
                (e1.span.0, e2.span.1),
            )),
        };
        result.map(|(e, s)| (SpExpr::new(e, (e1.span.0, e2.span.1)), s))
    }

    // Step an expression e, forward
    pub fn step(&mut self) {
        for (path, var, loc) in &self.eval_order {
            let expr = self.globals.get(&(path.clone(), var.clone())).unwrap();
            if self.mut_rec_streams.contains(&(path.clone(), var.clone())) {
                self.mut_rec_prevs.insert(*loc, expr.clone());
            }

            let (expr, temp_loc) = expr.replace_stream_loc(*loc).unwrap();
            let store_val = self.store.later.get(&temp_loc).cloned().unwrap();
            self.store.later.insert(*loc, store_val);

            let span = expr.span;
            let stepped_expr =
                SpExpr::new(Expr::Adv(SpExpr::new(Expr::Location(*loc), span)), span);
            self.globals
                .insert((path.clone(), var.clone()), stepped_expr);
        }

        self.update_store();
    }

    pub fn update_store(&mut self) {
        let mut to_dealloc: HashSet<u64> = self.store.now.keys().cloned().collect();
        for (_, _, loc) in &self.eval_order {
            to_dealloc.remove(loc);
        }
        self.store.now = self.store.later.clone();
        self.store.later = HashMap::new();

        self.allocator.dealloc_set(to_dealloc);
    }

    // Returns if there has been a match and any variable bindings
    fn match_pattern(
        val: &SpExpr,
        pattern: &Spanned<Pattern>,
    ) -> Result<(bool, Vec<(String, SpExpr)>), SpError> {
        let (pattern, p_span) = (pattern.term.clone(), pattern.span);
        use Pattern::*;
        match pattern.as_ref() {
            Underscore => Ok((true, Vec::with_capacity(0))),
            Bool(b1) => match val.term.as_ref() {
                Expr::Bool(b2) => {
                    if b1 == b2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Int(i1) => match val.term.as_ref() {
                Expr::Int(i2) => {
                    if i1 == i2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Float(f1) => match val.term.as_ref() {
                Expr::Float(f2) => {
                    if f1 == f2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            String(s1) => match val.term.as_ref() {
                Expr::String(s2) => {
                    if s1 == s2 {
                        Ok((true, Vec::with_capacity(0)))
                    } else {
                        Ok((false, Vec::with_capacity(0)))
                    }
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Unit => match val.term.as_ref() {
                Expr::Unit => Ok((true, Vec::with_capacity(0))),
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Tuple(patterns) => match val.term.as_ref() {
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
                        Err(SpError::new(
                            PatternMatchError(*pattern, *val.term.clone()).into(),
                            p_span,
                        ))
                    }
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            List(patterns) => match val.term.as_ref() {
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
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Variant(_, c1, o1) => match val.term.as_ref() {
                Expr::Variant(_, c2, o2) => {
                    if c1 != c2 {
                        return Ok((false, Vec::with_capacity(0)));
                    }
                    if o1.is_none() && o2.is_none() {
                        return Ok((true, Vec::with_capacity(0)));
                    }
                    if o1.is_some() && o2.is_some() {
                        let (p, v) = (o1.clone().unwrap(), o2.clone().unwrap());
                        return Self::match_pattern(&v, &p);
                    }
                    Err(SpError::new(
                        PatternMatchError(*pattern, *val.term.clone()).into(),
                        p_span,
                    ))
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Struct(path1, s1, patterns) => match val.term.as_ref() {
                Expr::Struct(path2, s2, vals) => {
                    if path1 != path2 {
                        return Err(SpError::new(
                            PatternMatchError(*pattern, *val.term.clone()).into(),
                            p_span,
                        ));
                    }
                    if s1 != s2 {
                        return Err(SpError::new(
                            PatternMatchError(*pattern, *val.term.clone()).into(),
                            p_span,
                        ));
                    }

                    let (mut matches, mut subs) = (true, Vec::new());
                    for (i, (field, o)) in patterns.into_iter().enumerate() {
                        if i >= vals.len() {
                            return Err(SpError::new(
                                PatternMatchError(*pattern, *val.term.clone()).into(),
                                p_span,
                            ));
                        }
                        if field == ".." {
                            break;
                        }

                        if field != &vals[i].0 {
                            return Err(SpError::new(
                                PatternMatchError(*pattern, *val.term.clone()).into(),
                                p_span,
                            ));
                        }

                        let (b, mut ss) = match o {
                            None => Self::match_pattern(
                                &vals[i].1,
                                &Spanned {
                                    term: Box::new(Var(field.clone(), false)),
                                    span: p_span,
                                },
                            )?,
                            Some(p) => Self::match_pattern(&vals[i].1, &p)?,
                        };

                        matches = matches && b;
                        subs.append(&mut ss);
                    }
                    Ok((matches, subs))
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Cons(p1, p2) => match val.term.as_ref() {
                Expr::List(vals) => {
                    let (b1, mut subs1) = Self::match_pattern(&vals[0], p1)?;
                    let tail = vals.range(1..).cloned().collect();
                    let (b2, mut subs2) = Self::match_pattern(
                        &SpExpr::new(Expr::List(tail), (vals[0].span.0, val.span.1)),
                        p2,
                    )?;

                    subs1.append(&mut subs2);
                    Ok((b1 && b2, subs1))
                }
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
            },
            Stream(p1, p2) => match val.term.as_ref() {
                Expr::Into(tuple) => match tuple.term.as_ref() {
                    Expr::Tuple(pair) => {
                        if pair.len() != 2 {
                            return Err(SpError::new(
                                PatternMatchError(*pattern, *val.term.clone()).into(),
                                p_span,
                            ));
                        }
                        if !matches!(pair[1].term.as_ref(), Expr::Location(_)) {
                            return Err(SpError::new(
                                PatternMatchError(*pattern, *val.term.clone()).into(),
                                p_span,
                            ));
                        }

                        let (b1, mut subs1) = Self::match_pattern(&pair[0], p1)?;
                        let (b2, mut subs2) = Self::match_pattern(&pair[1], p2)?;

                        subs1.append(&mut subs2);
                        Ok((b1 && b2, subs1))
                    }
                    _ => Err(SpError::new(
                        PatternMatchError(*pattern, *val.term.clone()).into(),
                        p_span,
                    )),
                },
                _ => Err(SpError::new(
                    PatternMatchError(*pattern, *val.term.clone()).into(),
                    p_span,
                )),
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

    fn extend(&mut self, loc: u64, term: SpExpr) {
        self.later.insert(loc, term);
    }

    fn now(&self) -> Self {
        Self {
            now: self.now.clone(),
            later: HashMap::new(),
        }
    }
}

impl SpExpr {
    // Return head and tail of stream if the expression is a stream
    fn is_stream(&self) -> Option<u64> {
        use Expr::{Into, Location, Tuple};

        match self.term.as_ref() {
            Into(tuple) => match tuple.term.as_ref() {
                Tuple(pair) => {
                    if pair.len() != 2 {
                        return None;
                    }
                    if let Location(loc) = pair[1].term.as_ref() {
                        Some(*loc)
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn replace_stream_loc(&self, loc: u64) -> Option<(SpExpr, u64)> {
        let span = self.span;
        use Expr::{Into, Location, Tuple};
        match self.term.as_ref() {
            Into(tuple) => match tuple.term.as_ref() {
                Tuple(pair) => {
                    if pair.len() != 2 {
                        return None;
                    }
                    if let Location(l) = pair[1].term.as_ref() {
                        Some((
                            SpExpr::new(
                                Into(SpExpr::new(
                                    Tuple(vec![pair[0].clone(), SpExpr::new(Location(loc), span)]),
                                    span,
                                )),
                                span,
                            ),
                            *l,
                        ))
                    } else {
                        return None;
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn step_value(&self) -> SpExpr {
        let span = self.span;

        use Expr::*;
        match self.term.as_ref() {
            Bool(_) | Int(_) | Float(_) | String(_) | Unit | Var(..) | Location(_) | Fn(..)
            | Fix(..) => self.clone(),
            Stable(v) => SpExpr::new(Stable(v.step_value()), span),
            Into(v) => match v.term.as_ref() {
                Tuple(pair) => {
                    if pair.len() == 2 {
                        if let Location(loc) = *pair[1].term {
                            SpExpr::new(Adv(SpExpr::new(Location(loc), span)), span)
                        } else {
                            SpExpr::new(Into(v.step_value()), span)
                        }
                    } else {
                        SpExpr::new(Into(v.step_value()), span)
                    }
                }
                _ => SpExpr::new(Into(v.step_value()), span),
            },
            List(list) => SpExpr::new(List(list.iter().map(|v| v.step_value()).collect()), span),
            Tuple(tuple) => {
                SpExpr::new(Tuple(tuple.iter().map(|v| v.step_value()).collect()), span)
            }
            Struct(path, name, fields) => SpExpr::new(
                Struct(
                    path.clone(),
                    name.clone(),
                    fields
                        .iter()
                        .map(|(f, v)| (f.clone(), v.step_value()))
                        .collect(),
                ),
                span,
            ),
            Variant(path, name, v) => SpExpr::new(
                Variant(
                    path.clone(),
                    name.clone(),
                    v.clone().map(|v| v.step_value()),
                ),
                span,
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
