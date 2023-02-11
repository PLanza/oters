mod errors;
mod tests;
mod utils;

use crate::errors::SpError;
use crate::parser::ast::{Opcode, PExpr, Pattern};
use crate::parser::span::{SpPExpr, Spanned};
use crate::types::{Type, TypeError};

use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use std::rc::Rc;

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
    Let(Spanned<Pattern>, SpExpr),
    LetAndWith(String, SpExpr, String, SpExpr, SpExpr),
    Use(Vec<String>, String),
}

pub type SpExpr = Spanned<Expr>;

// The actual expressions of the language.
// These don't include top-level expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    BinOp(SpExpr, BOpcode, SpExpr),
    UnOp(UOpcode, SpExpr),
    Delay(SpExpr),  // From Patrick Bahr's Rattus
    Stable(SpExpr), // From Patrick Bahr's Rattus
    Adv(SpExpr),    // From Patrick Bahr's Rattus
    Unbox(SpExpr),  // From Patrick Bahr's Rattus
    Out(SpExpr),    // From Patrick Bahr's Rattus
    Into(SpExpr),   // From Patrick Bahr's Rattus
    List(VecDeque<SpExpr>),
    Tuple(Vec<SpExpr>),
    Struct(Vec<String>, String, Vec<(String, SpExpr)>),
    Variant(Vec<String>, String, Option<SpExpr>),
    Fn(Spanned<Pattern>, SpExpr),
    Fix(String, SpExpr), // From Patrick Bahr's Rattus
    If(SpExpr, SpExpr, SpExpr),
    Seq(SpExpr, SpExpr),
    App(SpExpr, SpExpr),
    ProjStruct(SpExpr, String),
    Match(SpExpr, Vec<(Spanned<Pattern>, SpExpr)>),
    Var(Vec<String>, String),
    LetIn(Spanned<Pattern>, SpExpr, SpExpr),
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

impl SpExpr {
    pub fn new(e: Expr, span: (usize, usize)) -> Self {
        Spanned {
            span,
            term: Box::new(e),
        }
    }
    // Type context and declarations only needed for the Fn expr and so a reference is sufficient
    pub fn from_pexpr(pe: SpPExpr) -> Result<SpExpr, SpError> {
        let (pe, span) = (pe.term, pe.span);
        match *pe {
            PExpr::Bool(b) => Ok(SpExpr::new(Expr::Bool(b), span)),
            PExpr::Int(i) => Ok(SpExpr::new(Expr::Int(i), span)),
            PExpr::Float(f) => Ok(SpExpr::new(Expr::Float(f), span)),
            PExpr::String(s) => Ok(SpExpr::new(Expr::String(s), span)),
            PExpr::Unit => Ok(SpExpr::new(Expr::Unit, span)),
            // Desugar e1 << e2 into Into (e1, e2)
            PExpr::BinOp(e1, Opcode::Stream, e2) => Ok(SpExpr::new(
                Expr::Into(SpExpr::new(
                    Expr::Tuple(vec![SpExpr::from_pexpr(e1)?, SpExpr::from_pexpr(e2)?]),
                    span,
                )),
                span,
            )),
            PExpr::BinOp(e1, op, e2) => Ok(SpExpr::new(
                Expr::BinOp(
                    SpExpr::from_pexpr(e1)?,
                    op.to_bopcode(),
                    SpExpr::from_pexpr(e2)?,
                ),
                span,
            )),
            PExpr::UnOp(Opcode::Neg, e) => Ok(SpExpr::new(
                Expr::UnOp(UOpcode::Neg, SpExpr::from_pexpr(e)?),
                span,
            )),
            PExpr::UnOp(Opcode::Not, e) => Ok(SpExpr::new(
                Expr::UnOp(UOpcode::Not, SpExpr::from_pexpr(e)?),
                span,
            )),
            PExpr::UnOp(Opcode::Delay, e) => {
                Ok(SpExpr::new(Expr::Delay(SpExpr::from_pexpr(e)?), span))
            }
            PExpr::UnOp(Opcode::Stable, e) => {
                Ok(SpExpr::new(Expr::Stable(SpExpr::from_pexpr(e)?), span))
            }
            PExpr::UnOp(Opcode::Adv, e) => Ok(SpExpr::new(Expr::Adv(SpExpr::from_pexpr(e)?), span)),
            PExpr::UnOp(Opcode::Unbox, e) => {
                Ok(SpExpr::new(Expr::Unbox(SpExpr::from_pexpr(e)?), span))
            }
            PExpr::UnOp(Opcode::Into, e) => {
                Ok(SpExpr::new(Expr::Into(SpExpr::from_pexpr(e)?), span))
            }
            PExpr::UnOp(Opcode::Out, e) => Ok(SpExpr::new(Expr::Out(SpExpr::from_pexpr(e)?), span)),
            PExpr::List(v) => {
                let mut list = VecDeque::new();
                for e in v {
                    list.push_back(SpExpr::from_pexpr(e)?);
                }

                Ok(SpExpr::new(Expr::List(list), span))
            }
            PExpr::StructExpr(path, name, v) => {
                let mut fields = Vec::new();
                for (f, e) in v {
                    fields.push((f, SpExpr::from_pexpr(e)?));
                }

                Ok(SpExpr::new(Expr::Struct(path, name, fields), span))
            }
            PExpr::Tuple(v) => {
                let mut tuple = Vec::new();
                for e in v {
                    tuple.push(SpExpr::from_pexpr(e)?);
                }

                Ok(SpExpr::new(Expr::Tuple(tuple), span))
            }
            PExpr::Fn(args, e) => {
                let mut e = *SpExpr::from_pexpr(e)?.term;
                for pat in args.iter().rev() {
                    e = Expr::Fn(pat.clone(), SpExpr::new(e, span));
                }

                Ok(SpExpr::new(e, span))
            }
            PExpr::If(e1, e2, e3) => Ok(SpExpr::new(
                Expr::If(
                    SpExpr::from_pexpr(e1)?,
                    SpExpr::from_pexpr(e2)?,
                    SpExpr::from_pexpr(e3)?,
                ),
                span,
            )),
            PExpr::Block(v) => {
                if v.len() == 1 {
                    return SpExpr::from_pexpr(v[0].clone());
                }

                let head = v[0].clone();
                let tail = v[1..].into();

                match head.term.as_ref() {
                    // Convert {let x = e1; e2} to let x = e1 in e2
                    PExpr::Let(pat, e) => {
                        let expr = SpExpr::from_pexpr(e.clone())?;

                        // Recursive variables are translated to fix points to ensure guarded recursion...
                        let (mut rec_e, mut is_rec) = (expr.clone(), false);
                        for var in pat.vars() {
                            let fix_var = format!("_rec{}", var);
                            let temp = rec_e.clone().substitute(
                                &var,
                                &SpExpr::new(
                                    Expr::Adv(SpExpr::new(
                                        Expr::Unbox(SpExpr::new(
                                            Expr::Var(Vec::new(), fix_var.clone()),
                                            expr.span,
                                        )),
                                        expr.span,
                                    )),
                                    span,
                                ),
                            );
                            is_rec |= temp.0;
                            rec_e = if temp.0 {
                                SpExpr::new(Expr::Fix(fix_var, temp.1), span)
                            } else {
                                rec_e
                            };
                        }

                        match pat.term.as_ref() {
                            Pattern::Var(var, _) => {
                                // If e is "traditionally" recursive in the same time step
                                if matches!(expr.term.as_ref(), Expr::Fn(..))
                                    && expr.is_static_recursive(&var)
                                {
                                    return Ok(SpExpr::new(
                                        Expr::LetIn(
                                            pat.clone(),
                                            expr.clone(),
                                            SpExpr::from_pexpr(Spanned {
                                                term: Box::new(PExpr::Block(tail)),
                                                span: (expr.span.1, span.0),
                                            })?,
                                        ),
                                        span,
                                    ));
                                }
                            }
                            _ => (),
                        };

                        if is_rec {
                            // ...but only if they're guarded recursive expressions
                            Ok(SpExpr::new(
                                Expr::LetIn(
                                    pat.clone(),
                                    rec_e,
                                    SpExpr::from_pexpr(Spanned {
                                        term: Box::new(PExpr::Block(tail)),
                                        span: (expr.span.1, span.0),
                                    })?,
                                ),
                                span,
                            ))
                        } else {
                            Ok(SpExpr::new(
                                Expr::LetIn(
                                    pat.clone(),
                                    expr.clone(),
                                    SpExpr::from_pexpr(Spanned {
                                        term: Box::new(PExpr::Block(tail)),
                                        span: (expr.span.1, span.0),
                                    })?,
                                ),
                                span,
                            ))
                        }
                    }
                    _ => Ok(SpExpr::new(
                        Expr::Seq(
                            SpExpr::from_pexpr(head.clone())?,
                            SpExpr::from_pexpr(Spanned {
                                term: Box::new(PExpr::Block(tail)),
                                span: (head.span.1, span.0),
                            })?,
                        ),
                        span,
                    )),
                }
            }
            PExpr::App(e1, e2) => Ok(SpExpr::new(
                Expr::App(SpExpr::from_pexpr(e1)?, SpExpr::from_pexpr(e2)?),
                span,
            )),
            PExpr::ProjStruct(e, s) => Ok(SpExpr::new(
                Expr::ProjStruct(SpExpr::from_pexpr(e)?, s),
                span,
            )),
            PExpr::Variant(path, name, o) => match o {
                None => Ok(SpExpr::new(Expr::Variant(path, name, None), span)),
                Some(e) => Ok(SpExpr::new(
                    Expr::Variant(path, name, Some(SpExpr::from_pexpr(e)?)),
                    span,
                )),
            },
            PExpr::Match(e, v) => {
                let mut p_es = Vec::new();
                for (p, e) in v {
                    p_es.push((p, SpExpr::from_pexpr(e)?));
                }
                Ok(SpExpr::new(Expr::Match(SpExpr::from_pexpr(e)?, p_es), span))
            }
            PExpr::Var(path, var) => Ok(SpExpr::new(Expr::Var(path, var), span)),
            _ => unreachable!(),
        }
    }

    // Substitues var for term, returning true if substitution took place
    pub fn substitute(self, var: &String, term: &SpExpr) -> (bool, SpExpr) {
        use Expr::*;
        let (e, span) = (*self.term.clone(), self.span);
        match e {
            Bool(_) | Int(_) | Float(_) | String(_) | Unit | Location(_) => (false, self),
            BinOp(e1, op, e2) => {
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);

                (b1 || b2, SpExpr::new(Expr::BinOp(e1_, op, e2_), span))
            }
            UnOp(op, e) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(UnOp(op, e_), span))
            }
            Delay(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(Delay(e_), span))
            }
            Stable(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(Stable(e_), span))
            }
            Adv(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(Adv(e_), span))
            }
            Unbox(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(Unbox(e_), span))
            }
            Into(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(Into(e_), span))
            }
            Out(e) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(Out(e_), span))
            }
            List(v) => {
                let mut list = VecDeque::new();
                let mut b = false;
                for e in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    list.push_back(e_);
                }

                (b, SpExpr::new(List(list), span))
            }
            Struct(path, id, v) => {
                let mut fields = Vec::new();
                let mut b = false;
                for (f, e) in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    fields.push((f, e_));
                }

                (b, SpExpr::new(Struct(path, id, fields), span))
            }
            Tuple(v) => {
                let mut tuple = Vec::new();
                let mut b = false;
                for e in v {
                    let (b_, e_) = e.substitute(var, term);
                    b = b || b_;
                    tuple.push(e_);
                }

                (b, SpExpr::new(Tuple(tuple), span))
            }
            Fn(pat, e) => {
                // Tighter binding variable
                if pat.contains(var) {
                    return (false, self);
                }
                let (b, e_) = e.substitute(var, term);
                (b, SpExpr::new(Fn(pat, e_), span))
            }
            Fix(alpha, e) => {
                // Tighter binding variable
                if alpha == var.clone() {
                    return (false, self);
                }
                let (b, e_) = e.substitute(var, term);
                (b, SpExpr::new(Fix(alpha, e_), span))
            }
            If(e1, e2, e3) => {
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);
                let (b3, e3_) = e3.substitute(var, term);

                (b1 || b2 || b3, SpExpr::new(If(e1_, e2_, e3_), span))
            }
            Seq(e1, e2) => {
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);

                (b1 || b2, SpExpr::new(Seq(e1_, e2_), span))
            }
            App(e1, e2) => {
                let (b1, e1_) = e1.substitute(var, term);
                let (b2, e2_) = e2.substitute(var, term);

                (b1 || b2, SpExpr::new(App(e1_, e2_), span))
            }
            ProjStruct(e, s) => {
                let (b, e_) = e.substitute(var, term);

                (b, SpExpr::new(ProjStruct(e_, s), span))
            }
            Variant(path, id, o) => match o {
                Some(e) => {
                    let (b, e_) = e.substitute(var, term);
                    (b, SpExpr::new(Variant(path, id, Some(e_)), span))
                }
                None => (false, SpExpr::new(Variant(path, id, None), span)),
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
                    patterns.push((p, e_p_));
                }

                (b, SpExpr::new(Match(e_, patterns), span))
            }
            Var(_, s) => {
                if s == var.clone() {
                    (true, term.clone())
                } else {
                    (false, self)
                }
            }
            LetIn(pat, e1, e2) => {
                // Tighter binding variable
                if pat.contains(var) {
                    (false, self)
                } else {
                    let (b1, e1_) = e1.substitute(var, term);
                    let (b2, e2_) = e2.substitute(var, term);
                    (b1 || b2, SpExpr::new(LetIn(pat, e1_, e2_), span))
                }
            }
        }
    }

    // convert expression to λ✓ as stated in Rattus
    pub fn single_tick(&self, new_vs: u32) -> SpExpr {
        let span = self.span;
        use Expr::*;
        match self.term.as_ref() {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) | Var(..) => self.clone(),
            BinOp(e1, op, e2) => SpExpr::new(
                BinOp(e1.single_tick(new_vs), *op, e2.single_tick(new_vs)),
                span,
            ),
            UnOp(op, e1) => SpExpr::new(UnOp(*op, e1.single_tick(new_vs)), span),
            // Rule 1
            Delay(e) => {
                let (r, sub_e) = e.sub_single_tick(&format!("_d{}", new_vs), false);
                match r {
                    None => SpExpr::new(Delay(e.single_tick(new_vs)), span),
                    Some(r_e) => SpExpr::new(
                        LetIn(
                            Spanned {
                                term: Box::new(Pattern::Var(format!("_d{}", new_vs), false)),
                                span: r_e.span,
                            },
                            r_e,
                            SpExpr::new(Delay(sub_e), span).single_tick(new_vs + 1),
                        ),
                        span,
                    ),
                }
            }
            Stable(e) => SpExpr::new(Stable(e.single_tick(new_vs)), span),
            Adv(e) => SpExpr::new(Adv(e.single_tick(new_vs)), span),
            Unbox(e) => SpExpr::new(Unbox(e.single_tick(new_vs)), span),
            Out(e) => SpExpr::new(Out(e.single_tick(new_vs)), span),
            Into(e) => SpExpr::new(Into(e.single_tick(new_vs)), span),
            List(v) => {
                let mut list = VecDeque::new();
                for e in v {
                    list.push_back(e.single_tick(new_vs));
                }

                SpExpr::new(List(list), span)
            }
            Tuple(v) => {
                let mut list = Vec::new();
                for e in v {
                    list.push(e.single_tick(new_vs));
                }

                SpExpr::new(Tuple(list), span)
            }
            Struct(path, str, fields) => {
                let mut _fields = Vec::new();
                for (field, e) in fields {
                    _fields.push((field.clone(), e.single_tick(new_vs)));
                }

                SpExpr::new(Struct(path.clone(), str.clone(), _fields), span)
            }
            Variant(path, id, o) => match o {
                None => self.clone(),
                Some(e) => SpExpr::new(
                    Variant(path.clone(), id.clone(), Some(e.single_tick(new_vs))),
                    span,
                ),
            },
            // Rule 2
            Fn(pat, e) => {
                let (r, sub_e) = e.sub_single_tick(&format!("_d{}", new_vs), true);
                match r {
                    None => SpExpr::new(Fn(pat.clone(), e.single_tick(new_vs)), span),
                    Some(r_e) => SpExpr::new(
                        LetIn(
                            Spanned {
                                term: Box::new(Pattern::Var(format!("_d{}", new_vs), false)),
                                span: pat.span,
                            },
                            SpExpr::new(Adv(r_e.clone()), r_e.span),
                            SpExpr::new(Fn(pat.clone(), sub_e), e.span).single_tick(new_vs + 1),
                        ),
                        span,
                    ),
                }
            }
            Fix(var, e) => SpExpr::new(Fix(var.clone(), e.single_tick(new_vs)), span),
            If(e1, e2, e3) => SpExpr::new(
                If(
                    e1.single_tick(new_vs),
                    e2.single_tick(new_vs),
                    e3.single_tick(new_vs),
                ),
                span,
            ),
            Seq(e1, e2) => SpExpr::new(Seq(e1.single_tick(new_vs), e2.single_tick(new_vs)), span),
            App(e1, e2) => SpExpr::new(App(e1.single_tick(new_vs), e2.single_tick(new_vs)), span),
            ProjStruct(e, field) => {
                SpExpr::new(ProjStruct(e.single_tick(new_vs), field.clone()), span)
            }
            Match(e, patterns) => {
                let ret_e = e.single_tick(new_vs);
                let mut _patterns = Vec::new();
                for (p, e) in patterns {
                    _patterns.push((p.clone(), e.single_tick(new_vs)));
                }

                SpExpr::new(Match(ret_e, _patterns), span)
            }
            LetIn(var, e1, e2) => SpExpr::new(
                LetIn(var.clone(), e1.single_tick(new_vs), e2.single_tick(new_vs)),
                span,
            ),
            Location(_) => unreachable!("Locations should only appear during interpretation"),
        }
    }

    // Makes adv substitutions for the single tick conversion
    // Returns the an option containing the expression that has been removed and
    // self with the substitution made
    pub fn sub_single_tick(&self, sub_var: &String, for_fn: bool) -> (Option<SpExpr>, SpExpr) {
        let span = self.span;
        use Expr::*;
        match self.term.as_ref() {
            Unit | Int(_) | Float(_) | Bool(_) | String(_) => (None, self.clone()),
            // Note we only make one substitution
            // So we stop the recursion if we have a return value
            BinOp(e1, op, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, SpExpr::new(BinOp(e1, *op, e2.clone()), span))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, SpExpr::new(BinOp(e1, *op, e2), span))
                }
            }
            UnOp(op, e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, SpExpr::new(UnOp(*op, e), span))
            }
            Delay(_) | Stable(_) | Fix(_, _) | Fn(_, _) | Var(..) => (None, self.clone()),
            Adv(e) => match e.term.as_ref() {
                Var(..) => (None, self.clone()),
                _ => {
                    if for_fn {
                        (
                            Some(e.clone()),
                            SpExpr::new(Var(Vec::new(), sub_var.clone()), span),
                        )
                    } else {
                        (
                            Some(e.clone()),
                            SpExpr::new(
                                Adv(SpExpr::new(Var(Vec::new(), sub_var.clone()), e.span)),
                                span,
                            ),
                        )
                    }
                }
            },
            Unbox(e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, SpExpr::new(Unbox(e), span))
            }
            Out(e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, SpExpr::new(Out(e), span))
            }
            Into(e) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, SpExpr::new(Into(e), span))
            }
            List(v) => {
                let mut ret_es = VecDeque::new();
                let mut r_ret = None;
                for e in v {
                    if matches!(r_ret, None) {
                        let (r, e) = e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push_back(e);
                    } else {
                        ret_es.push_back(e.clone());
                    }
                }
                (r_ret, SpExpr::new(List(ret_es), span))
            }
            Tuple(v) => {
                let mut ret_es = Vec::new();
                let mut r_ret = None;
                for e in v {
                    if matches!(r_ret, None) {
                        let (r, e) = e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push(e);
                    } else {
                        ret_es.push(e.clone());
                    }
                }
                (r_ret, SpExpr::new(Tuple(ret_es), span))
            }
            Struct(path, str, v) => {
                let mut ret_es = Vec::new();
                let mut r_ret = None;
                for (f, e) in v {
                    if matches!(r_ret, None) {
                        let (r, e) = e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push((f.clone(), e));
                    } else {
                        ret_es.push((f.clone(), e.clone()));
                    }
                }
                (
                    r_ret,
                    SpExpr::new(Struct(path.clone(), str.clone(), ret_es), span),
                )
            }
            Variant(path, s, o) => match o {
                None => (None, self.clone()),
                Some(e) => {
                    let (r, e) = e.sub_single_tick(sub_var, for_fn);
                    (
                        r,
                        SpExpr::new(Variant(path.clone(), s.clone(), Some(e)), span),
                    )
                }
            },
            If(e1, e2, e3) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, SpExpr::new(If(e1, e2.clone(), e3.clone()), span))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    if !matches!(r2, None) {
                        (r2, SpExpr::new(If(e1, e2, e3.clone()), span))
                    } else {
                        let (r3, e3) = e3.sub_single_tick(sub_var, for_fn);
                        (r3, SpExpr::new(If(e1, e2, e3), span))
                    }
                }
            }
            Seq(e1, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, SpExpr::new(Seq(e1, e2.clone()), span))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, SpExpr::new(Seq(e1, e2), span))
                }
            }
            App(e1, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, SpExpr::new(App(e1, e2.clone()), span))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, SpExpr::new(App(e1, e2), span))
                }
            }
            ProjStruct(e, f) => {
                let (r, e) = e.sub_single_tick(sub_var, for_fn);
                (r, SpExpr::new(ProjStruct(e, f.clone()), span))
            }
            Match(e, v) => {
                let (mut r_ret, e) = e.sub_single_tick(sub_var, for_fn);
                let mut ret_es = Vec::new();

                for (p, p_e) in v {
                    if matches!(r_ret, None) {
                        let (r, p_e) = p_e.sub_single_tick(sub_var, for_fn);
                        r_ret = r;
                        ret_es.push((p.clone(), p_e));
                    } else {
                        ret_es.push((p.clone(), p_e.clone()));
                    }
                }
                (r_ret, SpExpr::new(Match(e, ret_es), span))
            }
            LetIn(pat, e1, e2) => {
                let (r1, e1) = e1.sub_single_tick(sub_var, for_fn);
                if !matches!(r1, None) {
                    (r1, SpExpr::new(LetIn(pat.clone(), e1, e2.clone()), span))
                } else {
                    let (r2, e2) = e2.sub_single_tick(sub_var, for_fn);
                    (r2, SpExpr::new(LetIn(pat.clone(), e1, e2), span))
                }
            }
            Location(_) => unreachable!("Locations should only appear during interpretation"),
        }
    }

    // If a function is recursive in one timestep
    pub fn is_static_recursive(&self, name: &String) -> bool {
        use Expr::*;
        match self.term.as_ref() {
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
    pub fn stable(&self) -> anyhow::Result<Self> {
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
    pub fn one_tick(&self) -> anyhow::Result<Self> {
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
    pub fn pre_tick(&self) -> anyhow::Result<Self> {
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
    pub fn get_var(self, var: &String) -> anyhow::Result<(Type, bool)> {
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

impl Spanned<Pattern> {
    // Returns true if the pattern contains the variable var
    fn contains(&self, var: &String) -> bool {
        use Pattern::*;
        match self.term.as_ref() {
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
        match self.term.as_ref() {
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
