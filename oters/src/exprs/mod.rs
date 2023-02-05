mod errors;
mod tests;
mod utils;

use crate::parser::ast::{Opcode, PExpr, Pattern};
use crate::types::{Type, TypeError};

use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
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

#[derive(Clone, Debug, PartialEq)]
pub enum LetBinding {
    Let(Pattern, Expr),
    LetAndWith(String, Expr, String, Expr, Expr),
    Use(Vec<String>, String),
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
    Delay(Box<Expr>),  // From Patrick Bahr's Rattus
    Stable(Box<Expr>), // From Patrick Bahr's Rattus
    Adv(Box<Expr>),    // From Patrick Bahr's Rattus
    Unbox(Box<Expr>),  // From Patrick Bahr's Rattus
    Out(Box<Expr>),    // From Patrick Bahr's Rattus
    Into(Box<Expr>),   // From Patrick Bahr's Rattus
    List(VecDeque<Box<Expr>>),
    Tuple(Vec<Box<Expr>>),
    Struct(Vec<String>, String, Vec<(String, Box<Expr>)>),
    Variant(Vec<String>, String, Option<Box<Expr>>),
    Fn(Pattern, Box<Expr>),
    Fix(String, Box<Expr>), // From Patrick Bahr's Rattus
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    ProjStruct(Box<Expr>, String),
    Match(Box<Expr>, Vec<(Pattern, Box<Expr>)>),
    Var(Vec<String>, String),
    LetIn(Pattern, Box<Expr>, Box<Expr>),
    Location(u64), // Only created by the interpreter
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
            PExpr::Bool(b) => Ok(Expr::Bool(b)),
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
                Ok(Expr::UnOp(UOpcode::Not, Box::new(Expr::from_pexpr(*e)?)))
            }
            PExpr::UnOp(Opcode::Delay, e) => Ok(Expr::Delay(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Stable, e) => Ok(Expr::Stable(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Adv, e) => Ok(Expr::Adv(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Unbox, e) => Ok(Expr::Unbox(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Into, e) => Ok(Expr::Into(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::UnOp(Opcode::Out, e) => Ok(Expr::Out(Box::new(Expr::from_pexpr(*e)?))),
            PExpr::List(v) => {
                let mut list = VecDeque::new();
                for e in v {
                    list.push_back(Box::new(Expr::from_pexpr(*e)?));
                }

                Ok(Expr::List(list))
            }
            PExpr::StructExpr(path, name, v) => {
                let mut fields = Vec::new();
                for (f, e) in v {
                    fields.push((f, Box::new(Expr::from_pexpr(*e)?)));
                }

                Ok(Expr::Struct(path, name, fields))
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
                for pat in args.iter().rev() {
                    e = Expr::Fn(pat.clone(), Box::new(e));
                }

                Ok(e)
            }
            PExpr::If(e1, e2, e3) => Ok(Expr::If(
                Box::new(Expr::from_pexpr(*e1)?),
                Box::new(Expr::from_pexpr(*e2)?),
                Box::new(Expr::from_pexpr(*e3)?),
            )),
            PExpr::Block(v) => {
                if v.len() == 1 {
                    return Expr::from_pexpr(*v[0].clone());
                }

                let head = v[0].clone();
                let tail = v[1..].into();

                match *head {
                    // Convert {let x = e1; e2} to let x = e1 in e2
                    PExpr::Let(pat, e) => {
                        let expr = Expr::from_pexpr(*e.clone())?;

                        // Recursive variables are translated to fix points to ensure guarded recursion...
                        let (mut rec_e, mut is_rec) = (expr.clone(), false);
                        for var in pat.vars() {
                            let fix_var = format!("_rec{}", var);
                            let temp = rec_e.clone().substitute(
                                &var,
                                &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(
                                    Vec::new(),
                                    fix_var.clone(),
                                ))))),
                            );
                            is_rec |= temp.0;
                            rec_e = if temp.0 {
                                Expr::Fix(fix_var, Box::new(temp.1))
                            } else {
                                rec_e
                            };
                        }

                        match pat.clone() {
                            Pattern::Var(var, _) => {
                                // If e is "traditionally" recursive in the same time step
                                if matches!(expr, Expr::Fn(..)) && expr.is_static_recursive(&var) {
                                    return Ok(Expr::LetIn(
                                        pat,
                                        Box::new(expr),
                                        Box::new(Expr::from_pexpr(PExpr::Block(tail))?),
                                    ));
                                }
                            }
                            _ => (),
                        };

                        if is_rec {
                            // ...but only if they're guarded recursive expressions
                            Ok(Expr::LetIn(
                                pat,
                                Box::new(rec_e),
                                Box::new(Expr::from_pexpr(PExpr::Block(tail))?),
                            ))
                        } else {
                            Ok(Expr::LetIn(
                                pat,
                                Box::new(expr),
                                Box::new(Expr::from_pexpr(PExpr::Block(tail))?),
                            ))
                        }
                    }
                    pe => Ok(Expr::Seq(
                        Box::new(Expr::from_pexpr(pe)?),
                        Box::new(Expr::from_pexpr(PExpr::Block(tail))?),
                    )),
                }
            }
            PExpr::App(e1, e2) => Ok(Expr::App(
                Box::new(Expr::from_pexpr(*e1)?),
                Box::new(Expr::from_pexpr(*e2)?),
            )),
            PExpr::ProjStruct(e, s) => Ok(Expr::ProjStruct(Box::new(Expr::from_pexpr(*e)?), s)),
            PExpr::Variant(path, name, o) => match o {
                None => Ok(Expr::Variant(path, name, None)),
                Some(e) => Ok(Expr::Variant(
                    path,
                    name,
                    Some(Box::new(Expr::from_pexpr(*e)?)),
                )),
            },
            PExpr::Match(e, v) => {
                let mut p_es = Vec::new();
                for (p, e) in v {
                    p_es.push((*p, Box::new(Expr::from_pexpr(*e)?)));
                }
                Ok(Expr::Match(Box::new(Expr::from_pexpr(*e)?), p_es))
            }
            PExpr::Var(path, var) => Ok(Expr::Var(path, var)),
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
                let mut list = VecDeque::new();
                let mut b = false;
                for e in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    list.push_back(Box::new(e_));
                }

                (b, List(list))
            }
            Struct(path, id, v) => {
                let mut fields = Vec::new();
                let mut b = false;
                for (f, e) in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    fields.push((f, Box::new(e_)));
                }

                (b, Struct(path, id, fields))
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
            Fn(pat, e) => {
                // Tighter binding variable
                if pat.contains(var) {
                    return (false, Fn(pat, e.clone()));
                }
                let (b, e_) = e.substitute(var, term);
                (b, Fn(pat, Box::new(e_)))
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
            Variant(path, id, o) => match o {
                Some(e) => {
                    let (b, e_) = e.substitute(var, term);
                    (b, Variant(path, id, Some(Box::new(e_))))
                }
                None => (false, Variant(path, id, None)),
            },
            Match(e, v) => {
                let (mut b, e_) = e.substitute(var, term);

                let mut patterns = Vec::new();
                for (p, e_p) in v {
                    // Tighter binding variable
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
            Var(path, s) => {
                if s == var.clone() {
                    (true, term.clone())
                } else {
                    (false, Var(path, s))
                }
            }
            LetIn(pat, e1, e2) => {
                // Tighter binding variable
                if pat.contains(var) {
                    (false, LetIn(pat, e1, e2))
                } else {
                    let (b1, e1_) = e1.substitute(var, term);
                    let (b2, e2_) = e2.substitute(var, term);
                    (b1 || b2, LetIn(pat, Box::new(e1_), Box::new(e2_)))
                }
            }
        }
    }

    // convert expression to λ✓ as stated in Rattus
    pub fn single_tick(&self, new_vs: u32) -> Expr {
        use Expr::*;
        match self {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) | Var(..) => self.clone(),
            BinOp(e1, op, e2) => BinOp(
                Box::new(e1.single_tick(new_vs)),
                *op,
                Box::new(e2.single_tick(new_vs)),
            ),
            UnOp(op, e1) => UnOp(*op, Box::new(e1.single_tick(new_vs))),
            // Rule 1
            Delay(e) => {
                let (r, sub_e) = e.sub_single_tick(&format!("_d{}", new_vs), false);
                match r {
                    None => Delay(Box::new(e.single_tick(new_vs))),
                    Some(r_e) => LetIn(
                        Pattern::Var(format!("_d{}", new_vs), false),
                        Box::new(r_e),
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
                let mut list = VecDeque::new();
                for e in v {
                    list.push_back(Box::new(e.single_tick(new_vs)));
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
            Struct(path, str, fields) => {
                let mut _fields = Vec::new();
                for (field, e) in fields {
                    _fields.push((field.clone(), Box::new(e.single_tick(new_vs))));
                }

                Struct(path.clone(), str.clone(), _fields)
            }
            Variant(path, id, o) => match o {
                None => self.clone(),
                Some(e) => Variant(
                    path.clone(),
                    id.clone(),
                    Some(Box::new(e.single_tick(new_vs))),
                ),
            },
            // Rule 2
            Fn(pat, e) => {
                let (r, sub_e) = e.sub_single_tick(&format!("_d{}", new_vs), true);
                match r {
                    None => Fn(pat.clone(), Box::new(e.single_tick(new_vs))),
                    Some(r_e) => LetIn(
                        Pattern::Var(format!("_d{}", new_vs), false),
                        Box::new(Adv(Box::new(r_e))),
                        Box::new(Fn(pat.clone(), Box::new(sub_e)).single_tick(new_vs + 1)),
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
            LetIn(var, e1, e2) => LetIn(
                var.clone(),
                Box::new(e1.single_tick(new_vs)),
                Box::new(e2.single_tick(new_vs)),
            ),
            Location(_) => unreachable!("Locations should only appear during interpretation"),
        }
    }

    // Makes adv substitutions for the single tick conversion
    // Returns the an option containing the expression that has been removed and
    // self with the substitution made
    pub fn sub_single_tick(&self, sub_var: &String, for_fn: bool) -> (Option<Expr>, Expr) {
        use Expr::*;
        match self {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) => (None, self.clone()),
            // Note we only make one substitution
            // So we stop the recursion if we have a return value
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
            Delay(_) | Stable(_) | Fix(_, _) | Fn(_, _) | Var(..) => (None, self.clone()),
            Adv(e) => match *e.clone() {
                Var(..) => (None, self.clone()),
                _ => {
                    if for_fn {
                        (Some(*e.clone()), Var(Vec::new(), sub_var.clone()))
                    } else {
                        (
                            Some(*e.clone()),
                            Adv(Box::new(Var(Vec::new(), sub_var.clone()))),
                        )
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
                let mut ret_es = VecDeque::new();
                let mut r_ret = None;
                for e in v {
                    if matches!(r_ret, None) {
                        let (r, e) = e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push_back(Box::new(e));
                    } else {
                        ret_es.push_back(e.clone());
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
            Struct(path, str, v) => {
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
                (r_ret, Struct(path.clone(), str.clone(), ret_es))
            }
            Variant(path, s, o) => match o {
                None => (None, self.clone()),
                Some(e) => {
                    let (r, e) = e.sub_single_tick(sub_var, for_fn);
                    (r, Variant(path.clone(), s.clone(), Some(Box::new(e))))
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
            LetIn(pat, e1, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, LetIn(pat.clone(), Box::new(e1), e2.clone()))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, LetIn(pat.clone(), Box::new(e1), Box::new(e2)))
                }
            }
            Location(_) => unreachable!("Locations should only appear during interpretation"),
        }
    }

    // If a function is recursive in one timestep
    pub fn is_static_recursive(&self, name: &String) -> bool {
        use Expr::*;
        match self {
            Bool(_) | Int(_) | Float(_) | String(_) | Unit | Location(_) => false,
            Delay(_) | Stable(_) | Adv(_) | Out(_) | Into(_) | Fix(..) => false,
            BinOp(e1, _, e2) => e1.is_static_recursive(name) || e2.is_static_recursive(name),
            UnOp(_, e) => e.is_static_recursive(name),
            Unbox(e) => e.is_static_recursive(name),
            List(es) => es.iter().fold(false, |mut acc, e| {
                acc = acc || e.is_static_recursive(name);
                acc
            }),
            Tuple(es) => es.iter().fold(false, |mut acc, e| {
                acc = acc || e.is_static_recursive(name);
                acc
            }),
            Struct(_, _, fs) => fs.iter().fold(false, |mut acc, f| {
                acc = acc || f.1.is_static_recursive(name);
                acc
            }),
            Variant(_, _, o) => match o {
                None => false,
                Some(e) => e.is_static_recursive(name),
            },
            Fn(pat, e) => !pat.contains(name) && e.is_static_recursive(name),
            If(e1, e2, e3) => {
                e1.is_static_recursive(name)
                    || e2.is_static_recursive(name)
                    || e3.is_static_recursive(name)
            }
            Seq(e1, e2) => e1.is_static_recursive(name) || e2.is_static_recursive(name),
            App(e1, e2) => e1.is_static_recursive(name) || e2.is_static_recursive(name),
            ProjStruct(e, _) => e.is_static_recursive(name),
            Match(e, ps) => {
                e.is_static_recursive(name)
                    || ps.iter().fold(false, |mut acc, p| {
                        acc = acc || (!p.0.contains(name) && p.1.is_static_recursive(name));
                        acc
                    })
            }
            Var(_, var) => var == name,
            LetIn(pat, e1, e2) => {
                !(pat.contains(name))
                    && (e1.is_static_recursive(name) || e2.is_static_recursive(name))
            }
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

    // Returns Γ^☐ of the context
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

    // Returns |Γ| of the context
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

    // Returns Γ for context Γ✓Γ'
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

    // Adds a variable to the context
    pub fn push_var(&mut self, var: String, t: Type) {
        self.terms
            .push(VarTerm::Var(Rc::new(RefCell::new((var, t)))))
    }

    // Adds a tick to the context
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

    // Applies a set of substitutions to the variables in the context
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

    // Returns the free variables of all the types in the context
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
    // Returns true if the pattern contains the variable var
    fn contains(&self, var: &String) -> bool {
        use Pattern::*;
        match self {
            Underscore | Bool(_) | Int(_) | Float(_) | String(_) | Unit => false,
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
            Variant(_, _, o) => match o {
                None => false,
                Some(p) => p.contains(var),
            },
            Struct(_, _, fields) => {
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
            Var(x, _) => x == var,
        }
    }

    // Returns all the variables in the pattern
    pub fn vars(&self) -> Vec<String> {
        use Pattern::*;
        match self {
            Underscore | Bool(_) | Int(_) | Float(_) | String(_) | Unit => Vec::new(),
            Tuple(ps) => {
                let mut ret = Vec::new();
                for p in ps {
                    ret.append(&mut p.vars());
                }
                ret
            }
            List(ps) => {
                let mut ret = Vec::new();
                for p in ps {
                    ret.append(&mut p.vars())
                }
                ret
            }
            Variant(_, _, o) => match o {
                None => Vec::new(),
                Some(p) => p.vars(),
            },
            Struct(_, _, fields) => {
                let mut ret = Vec::new();
                for (field, o) in fields {
                    match o {
                        None => {
                            if field != ".." {
                                ret.push(field.clone())
                            }
                        }
                        Some(p) => ret.append(&mut p.vars()),
                    }
                }
                ret
            }
            Cons(p1, p2) => {
                let mut ret = p1.vars();
                ret.append(&mut p2.vars());
                ret
            }
            Stream(p1, p2) => {
                let mut ret = p1.vars();
                ret.append(&mut p2.vars());
                ret
            }
            Or(p1, p2) => {
                let mut ret = p1.vars();
                ret.append(&mut p2.vars());
                ret
            }
            Var(x, _) => vec![x.clone()],
        }
    }
}
