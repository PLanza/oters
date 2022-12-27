mod errors;
mod utils;

use crate::parser::ast::{Opcode, PExpr, Pattern};
use crate::types::{Type, TypeError};

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use anyhow::{Ok, Result};

pub use errors::*;
pub use utils::*;

#[derive(Clone, Debug, PartialEq)]
pub enum VarTerm {
    Tick,
    Var(Rc<RefCell<(String, Type)>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarContext {
    pub(crate) terms: Vec<VarTerm>,
    pub(crate) ticks: Vec<usize>, // Locations of ticks within the context
}

// The actual expressions of the language.
// These don't include top-level expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    BinOp(Box<Expr>, BOpcode, Box<Expr>),
    UnOp(UOpcode, Box<Expr>),
    Delay(Box<Expr>),     // From Patrick Bahr's Rattus
    Stable(Box<Expr>),    // From Patrick Bahr's Rattus
    Adv(Box<Expr>),       // From Patrick Bahr's Rattus
    Unbox(Box<Expr>),     // From Patrick Bahr's Rattus
    Out(Box<Expr>),       // From Patrick Bahr's Rattus
    Into(Box<Expr>),      // From Patrick Bahr's Rattus
    List(Vec<Box<Expr>>), // Should change to other data structure
    Tuple(Vec<Box<Expr>>),
    Struct(String, Vec<(String, Box<Expr>)>),
    Variant(String, Option<Box<Expr>>),
    Fn((String, bool), Box<Expr>),
    Fix(String, Box<Expr>), // From Patrick Bahr's Rattus
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    ProjStruct(Box<Expr>, String),
    Match(Box<Expr>, Vec<(Pattern, Box<Expr>)>),
    Var(String),
    Let(String, Box<Expr>), // Should change to let x = t in e
    Location(u64),          // Only created by the interpreter
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BOpcode {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    Cons,
    Eq,
    Lt,
    Gt,
    And,
    Or,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UOpcode {
    Neg,
    Not,
}

impl Expr {
    // Type context and declarations only needed for the Fn expr and so a reference is sufficient
    pub fn from_pexpr(pe: PExpr) -> Result<Expr> {
        match pe {
            PExpr::True => Ok(Expr::Bool(true)),
            PExpr::False => Ok(Expr::Bool(false)),
            PExpr::Int(i) => Ok(Expr::Int(i)),
            PExpr::Float(f) => Ok(Expr::Float(f)),
            PExpr::String(s) => Ok(Expr::String(s)),
            PExpr::Unit => Ok(Expr::Unit),
            // Desugar e1 << e2 into Into (e1, e2)
            PExpr::BinOp(e1, Opcode::Stream, e2) => Ok(Expr::Into(Box::new(Expr::Tuple(vec![
                Box::new(Expr::from_pexpr(*e1)?),
                Box::new(Expr::from_pexpr(*e2)?),
            ])))),
            PExpr::BinOp(e1, op, e2) => Ok(Expr::BinOp(
                Box::new(Expr::from_pexpr(*e1)?),
                op.to_bopcode(),
                Box::new(Expr::from_pexpr(*e2)?),
            )),
            PExpr::UnOp(Opcode::Neg, e) => {
                Ok(Expr::UnOp(UOpcode::Neg, Box::new(Expr::from_pexpr(*e)?)))
            }
            PExpr::UnOp(Opcode::Not, e) => {
                Ok(Expr::UnOp(UOpcode::Neg, Box::new(Expr::from_pexpr(*e)?)))
            }
            PExpr::UnOp(Opcode::Delay, e) => Ok(Expr::Delay(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Stable, e) => Ok(Expr::Stable(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Adv, e) => Ok(Expr::Adv(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Unbox, e) => Ok(Expr::Unbox(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Into, e) => Ok(Expr::Into(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Out, e) => Ok(Expr::Out(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::List(v) => {
                let mut list = Vec::new();
                for e in v {
                    list.push(Box::new(Expr::from_pexpr(*e)?));
                }

                Ok(Expr::List(list))
            }
            PExpr::StructExpr(id, v) => {
                let mut fields = Vec::new();
                for (f, e) in v {
                    fields.push((f, Box::new(Expr::from_pexpr(*e)?)));
                }

                Ok(Expr::Struct(id, fields))
            }
            PExpr::Tuple(v) => {
                let mut tuple = Vec::new();
                for e in v {
                    tuple.push(Box::new(Expr::from_pexpr(*e)?));
                }

                Ok(Expr::Tuple(tuple))
            }
            PExpr::Fn(args, e) => {
                let mut e = Expr::from_pexpr(*e)?;
                for s in args.iter().rev() {
                    e = Expr::Fn(s.clone(), Box::new(e));
                }

                if args.is_empty() {
                    return Ok(Expr::Fn(("_".to_string(), true), Box::new(e)));
                }

                Ok(e)
            }
            PExpr::If(e1, e2, e3) => Ok(Expr::If(
                Box::new(Expr::from_pexpr(*e1)?),
                Box::new(Expr::from_pexpr(*e2)?),
                Box::new(Expr::from_pexpr(*e3)?),
            )),
            PExpr::Block(v) => {
                let mut v_iter = v.into_iter();
                // v will always be of length at least one
                let mut e = Expr::from_pexpr(*v_iter.next().unwrap())?;
                for e2 in v_iter {
                    e = Expr::Seq(Box::new(e), Box::new(Expr::from_pexpr(*e2)?));
                }

                Ok(e)
            }
            PExpr::App(e1, e2) => Ok(Expr::App(
                Box::new(Expr::from_pexpr(*e1)?),
                Box::new(Expr::from_pexpr(*e2)?),
            )),
            PExpr::ProjStruct(e, s) => Ok(Expr::ProjStruct(Box::new(Expr::from_pexpr(*e)?), s)),
            PExpr::Variant(id, o) => match o {
                None => Ok(Expr::Variant(id, None)),
                Some(e) => Ok(Expr::Variant(id, Some(Box::new(Expr::from_pexpr(*e)?)))),
            },
            PExpr::Match(e, v) => {
                let mut p_es = Vec::new();
                for (p, e) in v {
                    p_es.push((*p, Box::new(Expr::from_pexpr(*e)?)));
                }
                Ok(Expr::Match(Box::new(Expr::from_pexpr(*e)?), p_es))
            }
            PExpr::Var(s) => Ok(Expr::Var(s)),
            // Recursive variables are translated to fix points to ensure guarded recursion
            PExpr::Let(id, e) => {
                let expr = Expr::from_pexpr(*e.clone())?;

                let fix_var = format!("_rec{}", id);
                let (is_rec, rec_e) = expr.clone().substitute(
                    &id,
                    &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(fix_var.clone()))))),
                );
                if is_rec {
                    Ok(Expr::Let(
                        id.clone(),
                        Box::new(Expr::Fix(fix_var, Box::new(rec_e))),
                    ))
                } else {
                    Ok(Expr::Let(id, Box::new(expr)))
                }
            }
            _ => unreachable!(),
        }
    }

    // Substitues var for term, returning true if substitution took place
    pub fn substitute(self, var: &String, term: &Expr) -> (bool, Expr) {
        use Expr::*;
        match self {
            Bool(_) | Int(_) | Float(_) | String(_) | Unit | Location(_) => (false, self),
            BinOp(e1, op, e2) => {
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);

                (b1 || b2, BinOp(Box::new(e1_), op, Box::new(e2_)))
            }
            UnOp(op, e) => {
                let (b, e_) = e.substitute(var, term);

                (b, UnOp(op, Box::new(e_)))
            }
            Delay(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, Delay(Box::new(e_)))
            }
            Stable(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, Stable(Box::new(e_)))
            }
            Adv(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, Adv(Box::new(e_)))
            }
            Unbox(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, Unbox(Box::new(e_)))
            }
            Into(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, Into(Box::new(e_)))
            }
            Out(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, Out(Box::new(e_)))
            }
            List(v) => {
                let mut list = Vec::new();
                let mut b = false;
                for e in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    list.push(Box::new(e_));
                }

                (b, List(list))
            }
            Struct(id, v) => {
                let mut fields = Vec::new();
                let mut b = false;
                for (f, e) in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    fields.push((f, Box::new(e_)));
                }

                (b, Struct(id, fields))
            }
            Tuple(v) => {
                let mut tuple = Vec::new();
                let mut b = false;
                for e in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    tuple.push(Box::new(e_));
                }

                (b, Tuple(tuple))
            }
            Fn(arg, e) => {
                // Tighter binding variable
                if arg.0 == var.clone() {
                    return (false, Fn(arg, e.clone()));
                }
                let (b, e_) = e.substitute(var, term);
                (b, Fn(arg, Box::new(e_)))
            }
            Fix(alpha, e) => {
                // Tighter binding variable
                if alpha == var.clone() {
                    return (false, Fix(alpha, e.clone()));
                }
                let (b, e_) = e.substitute(var, term);
                (b, Fix(alpha, Box::new(e_)))
            }
            If(e1, e2, e3) => {
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);
                let (b3, e3_) = e3.substitute(var, term);

                (
                    b1 || b2 || b3,
                    If(Box::new(e1_), Box::new(e2_), Box::new(e3_)),
                )
            }
            Seq(e1, e2) => {
                match *e1.clone() {
                    Let(x, _) => {
                        if x == var.clone() {
                            return (false, Seq(e1, e2));
                        }
                    }
                    _ => (),
                }
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);

                (b1 || b2, Seq(Box::new(e1_), Box::new(e2_)))
            }
            App(e1, e2) => {
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);

                (b1 || b2, App(Box::new(e1_), Box::new(e2_)))
            }
            ProjStruct(e, s) => {
                let (b, e_) = e.substitute(var, term);

                (b, ProjStruct(Box::new(e_), s))
            }
            Variant(id, o) => match o {
                Some(e) => {
                    let (b, e_) = e.substitute(var, term);
                    (b, Variant(id, Some(Box::new(e_))))
                }
                None => (false, Variant(id, None)),
            },
            Match(e, v) => {
                let (mut b, e_) = e.substitute(var, term);

                let mut patterns = Vec::new();
                for (p, e_p) in v {
                    if p.contains(var) {
                        patterns.push((p, e_p));
                        continue;
                    }

                    let (b_p, e_p_) = e_p.substitute(var, term);
                    b = b || b_p;
                    patterns.push((p, Box::new(e_p_)));
                }

                (b, Match(Box::new(e_), patterns))
            }
            Var(s) => {
                if s == var.clone() {
                    (true, term.clone())
                } else {
                    (false, Var(s))
                }
            }
            Let(x, e) => {
                if x == var.clone() {
                    // Tighter binding variable
                    (false, Let(x, e))
                } else {
                    let (b, e_) = e.substitute(var, term);
                    (b, Let(x, Box::new(e_)))
                }
            }
        }
    }

    // convert expression to λ✓
    pub fn single_tick(&self, new_vs: u32) -> Expr {
        use Expr::*;
        match self {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) | Var(_) => self.clone(),
            BinOp(e1, op, e2) => BinOp(
                Box::new(e1.single_tick(new_vs)),
                *op,
                Box::new(e2.single_tick(new_vs)),
            ),
            UnOp(op, e1) => UnOp(*op, Box::new(e1.single_tick(new_vs))),
            Delay(e) => {
                let (r, sub_e) = e.sub_single_tick(&format!("_d{}", new_vs), false);
                match r {
                    None => Delay(Box::new(e.single_tick(new_vs))),
                    Some(r_e) => Seq(
                        Box::new(Let(format!("_d{}", new_vs), Box::new(r_e))),
                        Box::new(Delay(Box::new(sub_e)).single_tick(new_vs + 1)),
                    ),
                }
            }
            Stable(e) => Stable(Box::new(e.single_tick(new_vs))),
            Adv(e) => Adv(Box::new(e.single_tick(new_vs))),
            Unbox(e) => Unbox(Box::new(e.single_tick(new_vs))),
            Out(e) => Out(Box::new(e.single_tick(new_vs))),
            Into(e) => Into(Box::new(e.single_tick(new_vs))),
            List(v) => {
                let mut list = Vec::new();
                for e in v {
                    list.push(Box::new(e.single_tick(new_vs)));
                }

                List(list)
            }
            Tuple(v) => {
                let mut list = Vec::new();
                for e in v {
                    list.push(Box::new(e.single_tick(new_vs)));
                }

                Tuple(list)
            }
            Struct(str, fields) => {
                let mut _fields = Vec::new();
                for (field, e) in fields {
                    _fields.push((field.clone(), Box::new(e.single_tick(new_vs))));
                }

                Struct(str.clone(), _fields)
            }
            Variant(id, o) => match o {
                None => self.clone(),
                Some(e) => Variant(id.clone(), Some(Box::new(e.single_tick(new_vs)))),
            },
            Fn(var, e) => {
                let (r, sub_e) = e.sub_single_tick(&format!("_d{}", new_vs), true);
                match r {
                    None => Fn(var.clone(), Box::new(e.single_tick(new_vs))),
                    Some(r_e) => Seq(
                        Box::new(Let(format!("_d{}", new_vs), Box::new(Adv(Box::new(r_e))))),
                        Box::new(Fn(var.clone(), Box::new(sub_e)).single_tick(new_vs + 1)),
                    ),
                }
            }
            Fix(var, e) => Fix(var.clone(), Box::new(e.single_tick(new_vs))),
            If(e1, e2, e3) => If(
                Box::new(e1.single_tick(new_vs)),
                Box::new(e2.single_tick(new_vs)),
                Box::new(e3.single_tick(new_vs)),
            ),
            Seq(e1, e2) => Seq(
                Box::new(e1.single_tick(new_vs)),
                Box::new(e2.single_tick(new_vs)),
            ),
            App(e1, e2) => App(
                Box::new(e1.single_tick(new_vs)),
                Box::new(e2.single_tick(new_vs)),
            ),
            ProjStruct(e, field) => ProjStruct(Box::new(e.single_tick(new_vs)), field.clone()),
            Match(e, patterns) => {
                let ret_e = e.single_tick(new_vs);
                let mut _patterns = Vec::new();
                for (p, e) in patterns {
                    _patterns.push((p.clone(), Box::new(e.single_tick(new_vs))));
                }

                Match(Box::new(ret_e), _patterns)
            }
            Let(var, e) => Let(var.clone(), Box::new(e.single_tick(new_vs))),
            Location(_) => unreachable!("Locations should only appear during interpretation"),
        }
    }

    pub fn sub_single_tick(&self, sub_var: &String, for_fn: bool) -> (Option<Expr>, Expr) {
        use Expr::*;
        match self {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) => (None, self.clone()),
            BinOp(e1, op, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, BinOp(Box::new(e1), *op, e2.clone()))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, BinOp(Box::new(e1), *op, Box::new(e2)))
                }
            }
            UnOp(op, e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, UnOp(*op, Box::new(e)))
            }
            Delay(_) | Stable(_) | Fix(_, _) | Fn(_, _) | Var(_) => (None, self.clone()),
            Adv(e) => match *e.clone() {
                Var(_) => (None, self.clone()),
                _ => {
                    if for_fn {
                        (Some(*e.clone()), Var(sub_var.clone()))
                    } else {
                        (Some(*e.clone()), Adv(Box::new(Var(sub_var.clone()))))
                    }
                }
            },
            Unbox(e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, Unbox(Box::new(e)))
            }
            Out(e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, Out(Box::new(e)))
            }
            Into(e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, Into(Box::new(e)))
            }
            List(v) => {
                let mut ret_es = Vec::new();
                let mut r_ret = None;
                for e in v {
                    if matches!(r_ret, None) {
                        let (r, e) = e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push(Box::new(e));
                    } else {
                        ret_es.push(e.clone());
                    }
                }
                (r_ret, List(ret_es))
            }
            Tuple(v) => {
                let mut ret_es = Vec::new();
                let mut r_ret = None;
                for e in v {
                    if matches!(r_ret, None) {
                        let (r, e) = e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push(Box::new(e));
                    } else {
                        ret_es.push(e.clone());
                    }
                }
                (r_ret, Tuple(ret_es))
            }
            Struct(str, v) => {
                let mut ret_es = Vec::new();
                let mut r_ret = None;
                for (f, e) in v {
                    if matches!(r_ret, None) {
                        let (r, e) = e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push((f.clone(), Box::new(e)));
                    } else {
                        ret_es.push((f.clone(), e.clone()));
                    }
                }
                (r_ret, Struct(str.clone(), ret_es))
            }
            Variant(s, o) => match o {
                None => (None, self.clone()),
                Some(e) => {
                    let (r, e) = e.sub_single_tick(sub_var, for_fn);
                    (r, Variant(s.clone(), Some(Box::new(e))))
                }
            },
            If(e1, e2, e3) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, If(Box::new(e1), e2.clone(), e3.clone()))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    if !matches!(r2, None) {
                        (r2, If(Box::new(e1), Box::new(e2), e3.clone()))
                    } else {
                        let (r3, e3) = e3.sub_single_tick(sub_var, for_fn);
                        (r3, If(Box::new(e1), Box::new(e2), Box::new(e3)))
                    }
                }
            }
            Seq(e1, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, Seq(Box::new(e1), e2.clone()))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, Seq(Box::new(e1), Box::new(e2)))
                }
            }
            App(e1, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, App(Box::new(e1), e2.clone()))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, App(Box::new(e1), Box::new(e2)))
                }
            }
            ProjStruct(e, f) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, ProjStruct(Box::new(e), f.clone()))
            }
            Match(e, v) => {
                let (mut r_ret, e) = e.sub_single_tick(sub_var, for_fn);
                let mut ret_es = Vec::new();

                for (p, p_e) in v {
                    if matches!(r_ret, None) {
                        let (r, p_e) = p_e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push((p.clone(), Box::new(p_e)));
                    } else {
                        ret_es.push((p.clone(), p_e.clone()));
                    }
                }
                (r_ret, Match(Box::new(e), ret_es))
            }
            Let(var, e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, Let(var.clone(), Box::new(e)))
            }
            Location(_) => unreachable!("Locations should only appear during interpretation"),
        }
    }
}

impl Opcode {
    fn to_bopcode(self) -> BOpcode {
        use Opcode::*;
        match self {
            Mul => BOpcode::Mul,
            Div => BOpcode::Div,
            Add => BOpcode::Add,
            Sub => BOpcode::Sub,
            Mod => BOpcode::Mod,
            Cons => BOpcode::Cons,
            Eq => BOpcode::Eq,
            Lt => BOpcode::Lt,
            Gt => BOpcode::Gt,
            And => BOpcode::And,
            Or => BOpcode::Or,
            op => unreachable!("Can't convert {:?} to BOpcode", op),
        }
    }
}

impl VarContext {
    pub fn new() -> Self {
        Self {
            terms: Vec::new(),
            ticks: Vec::new(),
        }
    }

    pub fn stable(&self) -> Result<Self> {
        let mut terms = Vec::new();
        for term in &self.terms {
            match term {
                VarTerm::Var(cell) => {
                    let t = cell.try_borrow()?.1.clone();
                    if t.is_stable()? {
                        terms.push(term.clone());
                    }
                }
                VarTerm::Tick => (),
            }
        }

        Ok(VarContext {
            terms,
            ticks: Vec::new(),
        })
    }

    pub fn one_tick(&self) -> Result<Self> {
        match self.ticks.last() {
            None => Ok(self.clone()),
            Some(i) => {
                let mut pre_tick = self.pre_tick()?;
                pre_tick = pre_tick.stable()?;

                let mut terms = pre_tick.terms.clone();
                terms.append(&mut self.terms.clone()[*i..].to_vec());

                Ok(Self {
                    terms,
                    ticks: Vec::new(),
                })
            }
        }
    }

    pub fn pre_tick(&self) -> Result<Self> {
        if self.ticks.len() > 0 {
            let terms = self.terms.clone()[0..*self.ticks.last().unwrap()].to_vec();

            Ok(Self {
                terms,
                ticks: self.ticks[0..self.ticks.len() - 1].to_vec(),
            })
        } else {
            Err(InvalidExprError::ImproperAdvExpr.into())
        }
    }

    pub fn push_var(&mut self, var: String, t: Type) {
        self.terms
            .push(VarTerm::Var(Rc::new(RefCell::new((var, t)))))
    }

    pub fn push_tick(&mut self) {
        self.ticks.push(self.terms.len());
        self.terms.push(VarTerm::Tick);
    }

    // Returns type of var and if it's a legal access according to the typing rule
    pub fn get_var(self, var: &String) -> Result<(Type, bool)> {
        for (i, term) in self.terms.iter().rev().enumerate() {
            match term {
                VarTerm::Tick => (),
                VarTerm::Var(cell) => {
                    let (x, t) = cell.try_borrow()?.clone();

                    if &x == var {
                        if t.is_stable()? {
                            return Ok((t.clone(), true));
                        } else {
                            if self.ticks.len() < 1 || i > self.ticks[0] {
                                return Ok((t.clone(), true));
                            } else {
                                return Ok((t.clone(), false));
                            }
                        }
                    }
                }
            }
        }

        Err(TypeError::UnboundVariableError(var.clone()).into())
    }

    pub fn apply_subs(&mut self, subs: &Vec<(String, Type)>) {
        for (var, t) in subs {
            for term in &mut self.terms {
                match term {
                    VarTerm::Tick => (),
                    VarTerm::Var(cell) => {
                        let mut term = cell.try_borrow_mut().unwrap();
                        term.1 = term.1.sub_generic(&var, &t);
                    }
                }
            }
        }
    }

    pub fn get_free_vars(&self) -> HashSet<String> {
        let mut free_vars = HashSet::new();
        for term in &self.terms {
            match term {
                VarTerm::Tick => (),
                VarTerm::Var(cell) => {
                    let t = &cell.borrow().1;
                    free_vars.extend(t.get_free_vars());
                }
            }
        }

        free_vars
    }
}

impl Pattern {
    fn contains(&self, var: &String) -> bool {
        use Pattern::*;
        match self {
            Underscore | True | False | Int(_) | Float(_) | String(_) | Unit => false,
            Tuple(ps) => {
                let mut ret = false;
                for p in ps {
                    ret = ret || p.contains(var)
                }
                ret
            }
            List(ps) => {
                let mut ret = false;
                for p in ps {
                    ret = ret || p.contains(var)
                }
                ret
            }
            Variant(_, o) => match o {
                None => false,
                Some(p) => p.contains(var),
            },
            Struct(_, fields) => {
                let mut ret = false;
                for (field, o) in fields {
                    let b = match o {
                        None => var == field,
                        Some(p) => p.contains(var),
                    };

                    ret = ret || b;
                }
                ret
            }
            Cons(p1, p2) => p1.contains(var) || p2.contains(var),
            Stream(p1, p2) => p1.contains(var) || p2.contains(var),
            Or(p1, p2) => p1.contains(var) || p2.contains(var),
            Var(x) => x == var,
        }
    }
}
