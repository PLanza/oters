mod errors;

use crate::parser::ast::{Opcode, PExpr, Pattern};
use crate::types::{Type, TypeContext};

use std::collections::HashMap;

use anyhow::{Result, Ok};

pub use errors::InvalidExprError;

#[derive(Clone, Debug, PartialEq)]
pub enum VarTerm {
    Tick,
    Var(String, Type),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarContext {
    pub(crate) terms: Vec<VarTerm>,
    pub(crate) ticks: Vec<usize>, // Locations of ticks within the context
}

// The actual expressions of the language.
// These don't include top-level expressions
#[derive(Clone)]
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
    List(Vec<Box<Expr>>),
    Struct(String, Vec<(String, Box<Expr>)>),
    Tuple(Vec<Box<Expr>>),
    Fn(Vec<(String, Type)>, Box<Expr>),
    Fix(String, Box<Expr>), // From Patrick Bahr's Rattus
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Box<Expr>>),
    App(Box<Expr>, Box<Expr>),
    ProjTuple(Box<Expr>, i64),
    ProjStruct(Box<Expr>, String),
    Variant(String, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<(Pattern, Box<Expr>)>),
    Var(String),
    Let(String, Box<Expr>),
}

#[derive(Copy, Clone)]
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

#[derive(Clone)]
pub enum UOpcode {
    Neg,
    Not,
}

impl PExpr {
    // Type context and declarations only needed for the Fn expr and so a reference is sufficient
    pub fn to_expr(self, t_context: &TypeContext, t_decs: &HashMap<String, Type>) -> Result<Expr> {
        match self {
            PExpr::True => Ok(Expr::Bool(true)),
            PExpr::False => Ok(Expr::Bool(true)),
            PExpr::Int(i) => Ok(Expr::Int(i)),
            PExpr::Float(f) => Ok(Expr::Float(f)),
            PExpr::String(s) => Ok(Expr::String(s)),
            PExpr::Unit => Ok(Expr::Unit),
            // Desugar e1 << e2 into Into (e1, e2)
            PExpr::BinOp(e1, Opcode::Stream, e2) => Ok(Expr::Into(
                Box::new(Expr::Tuple(vec![
                    Box::new(e1.to_expr(t_context, t_decs)?),
                    Box::new(e2.to_expr(t_context, t_decs)?),
                ])),
            )),
            PExpr::BinOp(e1, op, e2) => Ok(Expr::BinOp(
                Box::new(e1.to_expr(t_context, t_decs)?), 
                op.to_bopcode(), 
                Box::new(e2.to_expr(t_context, t_decs)?)
            )),
            PExpr::UnOp(Opcode::Neg, e) => Ok(Expr::UnOp(UOpcode::Neg, Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::UnOp(Opcode::Not, e) => Ok(Expr::UnOp(UOpcode::Neg, Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::UnOp(Opcode::Delay, e) => Ok(Expr::Delay(Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::UnOp(Opcode::Stable, e) => Ok(Expr::Stable(Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::UnOp(Opcode::Adv, e) => Ok(Expr::Adv(Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::UnOp(Opcode::Unbox, e) => Ok(Expr::Unbox(Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::UnOp(Opcode::Into, e) => Ok(Expr::Into(Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::UnOp(Opcode::Out, e) => Ok(Expr::Out(Box::new(e.to_expr(t_context, t_decs)?))),
            PExpr::List(v) => {
                let mut list = Vec::new();
                for e in v {
                    list.push(Box::new(e.to_expr(t_context, t_decs)?));
                }

                Ok(Expr::List(list))
            }
            PExpr::StructExpr(id, v) => {
                let mut fields = Vec::new();
                for (f, e) in v {
                    fields.push((f, Box::new(e.to_expr(t_context, t_decs)?)));
                }

                Ok(Expr::Struct(id, fields))
            }
            PExpr::Tuple(v) => {
                let mut tuple = Vec::new();
                for e in v {
                    tuple.push(Box::new(e.to_expr(t_context, t_decs)?));
                }

                Ok(Expr::Tuple(tuple))
            }
            PExpr::Fn(args, e) => {
                let mut t_args = Vec::new();
                for (s, t) in args {
                    t_args.push((s.clone(), t.to_type(t_context.clone(), t_decs)?));
                }

                Ok(Expr::Fn(t_args, Box::new(e.to_expr(t_context, t_decs)?)))
            }
            PExpr::Fix(alpha, e) => {
                Ok(Expr::Fix(alpha, Box::new(e.to_expr(t_context, t_decs)?)))
            }
            PExpr::If(e1, e2, e3) => {
                Ok(Expr::If(
                    Box::new(e1.to_expr(t_context, t_decs)?), 
                    Box::new(e2.to_expr(t_context, t_decs)?),
                    Box::new(e3.to_expr(t_context, t_decs)?)
                ))
            }
            PExpr::Block(v) => {
                let mut exprs = Vec::new();
                for e in v {
                    exprs.push(Box::new(e.to_expr(t_context, t_decs)?));
                }

                Ok(Expr::Block(exprs))
            }
            PExpr::App(e1, e2) => Ok(Expr::App(
                Box::new(e1.to_expr(t_context, t_decs)?), 
                Box::new(e2.to_expr(t_context, t_decs)?)
            )),
            PExpr::ProjTuple(e, i) => Ok(Expr::ProjTuple(Box::new(e.to_expr(t_context, t_decs)?), i)),
            PExpr::ProjStruct(e, s) => Ok(Expr::ProjStruct(Box::new(e.to_expr(t_context, t_decs)?), s)),
            PExpr::Variant(id, o) => match o {
                None => Ok(Expr::Variant(id, None)),
                Some(e) => e.to_expr(t_context, t_decs)
                
            }
            PExpr::Match(e, v) => {
                let mut p_es = Vec::new();
                for (p, e) in v {
                    p_es.push((*p, Box::new(e.to_expr(t_context, t_decs)?)));
                }
                Ok(Expr::Match(Box::new(e.to_expr(t_context, t_decs)?), p_es))
            }
            PExpr::Var(s) => Ok(Expr::Var(s)),
            // Recursive functions are translated to fix points to ensure guarded recursion
            PExpr::Let(id, _, e) => match e.to_expr(t_context, t_decs)? {
                Expr::Fn(ts, fn_e) => {
                    let (is_rec, rec_e) = fn_e.clone().substitute(&id, 
                        &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(id.clone()))))));
                    if is_rec {
                        Ok(Expr::Let(
                            id.clone(),
                            Box::new(Expr::Fix(id, Box::new(Expr::Fn(ts, Box::new(rec_e))))
                        )))
                    } else {
                        Ok(Expr::Let(
                            id,
                            Box::new(Expr::Fn(ts, fn_e))
                        ))
                    }
                }
                e => Ok(Expr::Let(id, Box::new(e)))
            }
            _ => unreachable!(),
        }
    }
}

impl Expr {
    // Substitues var for val, returning true if substitution took place
    pub fn substitute(self, var: &String, val: &Expr) -> (bool, Expr) {
        use Expr::*;
        match self {
            Bool(_) => (false, self),
            Int(_) => (false, self),
            Float(_) => (false, self),
            String(_) => (false, self),
            Unit => (false, self),
            BinOp(e1, op, e2) => {
                let (b1, e1_) = e1.substitute(var, val);
                let (b2, e2_) = e2.substitute(var, val);

                (b1 || b2, BinOp(Box::new(e1_), op, Box::new(e2_)))
            },
            UnOp(op, e) => {
                let (b, e_) = e.substitute(var, val);

                (b, UnOp(op, Box::new(e_)))
            },
            Delay(e) => {
                let (b, e_) = e.substitute(var, val);

                (b, Delay(Box::new(e_)))
            }
            Stable(e) => {
                let (b, e_) = e.substitute(var, val);

                (b, Stable(Box::new(e_)))
            }
            Adv(e) => {
                let (b, e_) = e.substitute(var, val);

                (b, Adv(Box::new(e_)))
            }
            Unbox(e) => {
                let (b, e_) = e.substitute(var, val);

                (b, Unbox(Box::new(e_)))
            }
            Into(e) => {
                let (b, e_) = e.substitute(var, val);

                (b, Into(Box::new(e_)))
            }
            Out(e) => {
                let (b, e_) = e.substitute(var, val);

                (b, Out(Box::new(e_)))
            }
            List(v) => {
                let mut list = Vec::new();
                let mut b = false;
                for e in v {
                    let (b_, e_) = e.substitute(var, val);
                    b = b || b_;
                    list.push(Box::new(e_));
                }

                (b, List(list))
            }
            Struct(id, v) => {
                let mut fields = Vec::new();
                let mut b = false;
                for (f, e) in v {
                    let (b_, e_) = e.substitute(var, val);
                    b = b || b_;
                    fields.push((f, Box::new(e_)));
                }

                (b, Struct(id, fields))
            }
            Tuple(v) => {
                let mut tuple = Vec::new();
                let mut b = false;
                for e in v {
                    let (b_, e_) = e.substitute(var, val);
                    b = b || b_;
                    tuple.push(Box::new(e_));
                }

                (b, Tuple(tuple))
            }
            Fn(args, e) => {
                for (arg, _) in &args {
                    // Tighter binding variable
                    if arg == var {
                        return (false, Fn(args.clone(), e));
                    } 
                }
                let (b, e_) = e.substitute(var, val);
                (b, Fn(args, Box::new(e_)))
            }
            Fix(alpha, e) => {
                // Tighter binding variable
                if alpha == var.clone() {
                    return (false, Fix(alpha.clone(), e));
                } 
                let (b, e_) = e.substitute(var, val);
                (b, Fix(alpha, Box::new(e_)))
            }
            If(e1, e2, e3) => {
                let (b1, e1_) = e1.substitute(var, val);
                let (b2, e2_) = e2.substitute(var, val);
                let (b3, e3_) = e3.substitute(var, val);

                (b1 || b2 || b3, If(Box::new(e1_), Box::new(e2_), Box::new(e3_)))
            },
            Block(v) => {
                let mut exprs = Vec::new();
                let mut b = false;
                for e in v {
                    match *e.clone() {
                        // Tighter binding variable
                        Let(id, _) => if id == var.clone() {
                            break;
                        } 
                        _ => ()
                        
                    }

                    let (b_, e_) = e.substitute(var, val);
                    b = b || b_;
                    exprs.push(Box::new(e_));
                }

                (b, Block(exprs))
            }
            App(e1, e2) => {
                let (b1, e1_) = e1.substitute(var, val);
                let (b2, e2_) = e2.substitute(var, val);

                (b1 || b2, App(Box::new(e1_), Box::new(e2_)))
            },
            ProjTuple(e, i) => {
                let (b, e_) = e.substitute(var, val);

                (b, ProjTuple(Box::new(e_), i))
            },
            ProjStruct(e, s) => {
                let (b, e_) = e.substitute(var, val);

                (b, ProjStruct(Box::new(e_), s))
            },
            Variant(id, o) => match o {
                    Some(e) => {
                        let (b, e_) = e.substitute(var, val);
                        (b, Variant(id, Some(Box::new(e_))))
                    }
                    None => (false, Variant(id, None))
                }
            Match(e, v) => {
                let (mut b, e_) = e.substitute(var, val);

                let mut patterns = Vec::new();
                for (p, e_p) in v {
                    //TODO: do not substitute it p binds var
                    let (b_p, e_p_) = e_p.substitute(var, val);
                    b = b || b_p;
                    patterns.push((p, Box::new(e_p_)));
                }

                (b, Match(Box::new(e_), patterns))
            }
            Var(s) => if s == var.clone() {
                (true, val.clone())
            } else {
                (false, Var(s))
            }
            Let(x, e) => {
                if x == var.clone() {
                    // Tighter binding variable 
                    (false, Let(x, e))
                } else {
                    let (b, e_) = e.substitute(var, val);
                    (b, Let(x, Box::new(e_)))
                }
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
}

