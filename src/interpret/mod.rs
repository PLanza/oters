mod errors;

use std::collections::HashMap;

use self::errors::InterpretError::*;
use crate::exprs::{BOpcode, Expr, UOpcode};

use anyhow::Result;

pub struct Interpreter {
    new_loc: u64,
}

// Potentially change locations to be shared references
#[derive(Clone)]
pub struct Store {
    pub(super) now: HashMap<u64, Expr>,
    pub(super) later: HashMap<u64, Expr>,
}

impl Interpreter {
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
                let s_n = Store {
                    now: s.now.clone(),
                    later: HashMap::new(),
                };
                let (_e, _s_n) = self.eval(*e.clone(), s_n)?;

                match _e {
                    Location(l) => match _s_n.now.get(&l) {
                        None => Err(UnboundLocationError(format!("{:?}", e)).into()),
                        Some(term) => self.eval(
                            term.clone(),
                            Store {
                                now: _s_n.now,
                                later: s.later.clone(),
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

                Ok((List(tuple_vec), store))
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
                    Fn(var, expr) => {
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
            Match(..) => todo!(),
            Var(x) => Err(UnboundVariableError(x).into()),
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

    fn alloc(&mut self) -> u64 {
        let loc = self.new_loc;
        self.new_loc += 1;
        loc
    }
}

impl Store {
    fn extend(&mut self, loc: u64, term: Expr) {
        self.later.insert(loc, term);
    }
}
