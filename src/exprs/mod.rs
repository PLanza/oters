mod errors;

use crate::parser::ast::{Opcode, PExpr, Pattern};
use crate::types::{Type, TypeError};

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use anyhow::{Ok, Result};

pub use errors::*;

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
#[derive(Clone, Debug)]
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
    Tuple(Vec<Box<Expr>>),
    Struct(String, Vec<(String, Box<Expr>)>),
    Variant(String, Option<Box<Expr>>),
    Fn(String, Box<Expr>),
    Fix(String, Box<Expr>), // From Patrick Bahr's Rattus
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    ProjStruct(Box<Expr>, String),
    Match(Box<Expr>, Vec<(Pattern, Box<Expr>)>),
    Var(String),
    Let(String, Box<Expr>),
}

#[derive(Copy, Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum UOpcode {
    Neg,
    Not,
}

impl Expr {
    // Type context and declarations only needed for the Fn expr and so a reference is sufficient
    pub fn from_pexpr(pe: PExpr) -> Result<Expr> {
        match pe {
            PExpr::True => Ok(Expr::Bool(true)),
            PExpr::False => Ok(Expr::Bool(true)),
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
            // Recursive functions are translated to fix points to ensure guarded recursion
            PExpr::Let(id, e) => match Expr::from_pexpr(*e.clone())? {
                Expr::Fn(arg, fn_e) => {
                    let fix_var = format!("rec_{}", id);
                    let (is_rec, rec_e) = fn_e.clone().substitute(
                        &id,
                        &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(fix_var.clone()))))),
                    );
                    if is_rec {
                        Ok(Expr::Let(
                            id.clone(),
                            Box::new(Expr::Fix(fix_var, Box::new(Expr::Fn(arg, Box::new(rec_e))))),
                        ))
                    } else {
                        Ok(Expr::Let(id, Box::new(Expr::Fn(arg, fn_e))))
                    }
                }
                // If e_ is recursive and not a function, then fail
                e_ => {
                    if e_.clone().substitute(&id, &Expr::Unit).0 {
                        Err(InvalidExprError::IllegalRecursiveExpr(e.head_string()).into())
                    } else {
                        Ok(Expr::Let(id, Box::new(e_)))
                    }
                }
            },
            _ => unreachable!(),
        }
    }

    // Substitues var for val, returning true if substitution took place
    pub fn substitute(self, var: &String, term: &Expr) -> (bool, Expr) {
        use Expr::*;
        match self {
            Bool(_) => (false, self),
            Int(_) => (false, self),
            Float(_) => (false, self),
            String(_) => (false, self),
            Unit => (false, self),
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
                if arg == var.clone() {
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
                    //TODO: do not substitute if p binds var
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

    pub fn get_var(self, var: &String) -> Result<Type> {
        for (i, term) in self.terms.iter().rev().enumerate() {
            match term {
                VarTerm::Tick => (),
                VarTerm::Var(cell) => {
                    let term = cell.try_borrow()?.clone();
                    let (x, t) = (term.0, term.1);

                    if &x == var {
                        if t.is_stable()? {
                            return Ok(t.clone());
                        } else {
                            if self.ticks.len() < 1 || i > self.ticks[0] {
                                return Ok(t.clone());
                            } else {
                                return Err(TypeError::InvalidVariableAccess(var.clone()).into());
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
