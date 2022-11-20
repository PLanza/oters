use std::collections::{HashMap, VecDeque};

use super::{Type, TypeContext, TypeError};
use crate::exprs::{BOpcode, Expr, InvalidExprError, UOpcode, VarContext};
use crate::parser::ast::{PExpr, Program};

use anyhow::Result;

#[derive(Debug)]
pub struct ProgramChecker {
    // These represent the global top-level declarations
    value_decs: HashMap<String, Type>, // Maps top-level values (i.e. `let` exprs) to their type
    type_decs: HashMap<String, Type>,  // Maps `type`s, `enum`s and `struct`s to their definitions
    variant_map: HashMap<String, String>, // Maps variants to their corresponding `enum`

    fresh_type_var: i32,
    substitutions: Vec<(String, Type)>,
}

impl ProgramChecker {
    pub fn new() -> Self {
        ProgramChecker {
            value_decs: HashMap::new(),
            type_decs: HashMap::new(),
            variant_map: HashMap::new(),
            fresh_type_var: 0,
            substitutions: Vec::new(),
        }
    }

    pub fn type_check_program(&mut self, program: &Program) -> Result<()> {
        for expr in program {
            match expr.as_ref() {
                // Type Aliases
                PExpr::TypeDef(t_id, params, t) => {
                    // The type being aliased
                    let t = Type::from_typedef(
                        t_id.clone(),
                        params.clone(),
                        *t.clone(),
                        &self.type_decs,
                    )?;

                    // Check that the type is well formed
                    t.well_formed(TypeContext::new())?;

                    // Add it to the type declarations
                    self.type_decs.insert(t_id.clone(), t);
                }
                PExpr::EnumDef(id, params, variants) => {
                    let t = Type::from_enumdef(
                        id.clone(),
                        params.clone(),
                        variants.clone(),
                        &self.type_decs,
                    )?;

                    // Check the enum is well formed
                    t.well_formed(TypeContext::new())?;

                    // Add enum to the type declarations
                    self.type_decs.insert(id.clone(), t);

                    // Add variants to map
                    for (s, _) in variants {
                        self.variant_map.insert(s.clone(), id.clone());
                    }
                }
                PExpr::StructDef(id, params, fields) => {
                    let t = Type::from_structdef(
                        id.clone(),
                        params.clone(),
                        fields.clone(),
                        &self.type_decs,
                    )?;

                    // Check the struct is well formed
                    t.well_formed(TypeContext::new())?;

                    // Add struct to the type declarations
                    self.type_decs.insert(id.clone(), t);
                }
                PExpr::Let(id, expr) => {
                    // If e is a recursive function then convert it into a fix expression
                    let e = match Expr::from_pexpr(*expr.clone())? {
                        Expr::Fn(arg, fn_e) => {
                            let fix_var = format!("rec_{}", id);
                            let (is_rec, rec_e) = fn_e.clone().substitute(
                                &id,
                                &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(
                                    fix_var.clone(),
                                ))))),
                            );
                            if is_rec {
                                Expr::Fix(fix_var, Box::new(Expr::Fn(arg, Box::new(rec_e))))
                            } else {
                                Expr::Fn(arg, fn_e)
                            }
                        }
                        // If e_ is recursive and not a function, then fail
                        e_ => {
                            if e_.clone().substitute(&id, &Expr::Unit).0 {
                                return Err(InvalidExprError::IllegalRecursiveExpr(
                                    expr.head_string(),
                                )
                                .into());
                            } else {
                                e_
                            }
                        }
                    };

                    // Type check the expression
                    let t = self.infer(&e, VarContext::new())?;

                    // Make sure the resulting type is well formed
                    t.well_formed(TypeContext::new())?;

                    // Add it to the map of value declarations
                    self.value_decs.insert(id.clone(), t);
                }
                _ => return Err(InvalidExprError::InvalidTopLevelExpr(expr.head_string()).into()),
            }
        }

        println!("{:?}", self.type_decs);
        Ok(())
    }

    pub fn infer(&mut self, e: &Expr, mut ctx: VarContext) -> Result<Type> {
        use Expr::*;

        match e {
            Bool(_) => Ok(Type::Bool),
            Int(_) => Ok(Type::Int),
            Float(_) => Ok(Type::Float),
            String(_) => Ok(Type::String),
            Unit => Ok(Type::Unit),
            BinOp(e1, op, e2) => {
                let t1 = self.infer(e1, ctx.clone())?;
                let t2 = self.infer(e2, ctx.clone())?;
                let t3 = self.infer_binop(t1.clone(), op.clone(), t2.clone())?;

                let mut subs = match op {
                    BOpcode::Cons => unify(VecDeque::from([
                        (t2.clone(), Type::List(Box::new(t1.clone()))),
                        (t2.clone(), t3.clone()),
                    ])),
                    BOpcode::Eq | BOpcode::Lt | BOpcode::Gt => {
                        unify(VecDeque::from([(t1.clone(), t2.clone())]))
                    }
                    _ => unify(VecDeque::from([
                        (t1.clone(), t3.clone()),
                        (t2.clone(), t3.clone()),
                        (t1.clone(), t2.clone()),
                    ])),
                };
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t3)
            }
            UnOp(UOpcode::Neg, e) => {
                let t = self.infer(e, ctx.clone())?;
                let mut subs = unify(VecDeque::from([(t.clone(), Type::Int)]));
                self.substitutions.append(&mut subs);
                println!("{:?}", self.substitutions);
                ctx.apply_subs(&self.substitutions);

                Ok(Type::Int)
            }
            UnOp(UOpcode::Not, e) => {
                let t = self.infer(e, ctx.clone())?;
                let mut subs = unify(VecDeque::from([(t.clone(), Type::Bool)]));
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(Type::Bool)
            }
            Delay(e) => {
                let mut ctx = ctx.clone();
                ctx.push_tick();

                let t = self.infer(e, ctx)?;

                Ok(Type::Delay(Box::new(t)))
            }
            Stable(e) => {
                let ctx = ctx.stable()?;

                let t = self.infer(e, ctx)?;

                Ok(Type::Stable(Box::new(t)))
            }
            Adv(e) => {
                let mut ctx = ctx.pre_tick()?;

                let t = self.infer(e, ctx.clone())?;
                let t_ret = Type::GenericVar(self.fresh_type_var());

                let mut subs = unify(VecDeque::from([(
                    t.clone(),
                    Type::Delay(Box::new(t_ret.clone())),
                )]));
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t_ret)
            }
            Unbox(e) => {
                let t = self.infer(e, ctx.clone())?;
                let t_ret = Type::GenericVar(self.fresh_type_var());

                let mut subs = unify(VecDeque::from([(
                    t.clone(),
                    Type::Stable(Box::new(t_ret.clone())),
                )]));
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t_ret)
            }
            Out(e) => {
                let t = self.infer(e, ctx.clone())?;
                let a = Type::GenericVar(self.fresh_type_var());

                let fix_var = self.fresh_type_var();

                let mut subs = unify(VecDeque::from([(
                    t.clone(),
                    Type::Fix(fix_var.clone(), Box::new(a.clone())),
                )]));
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                match t {
                    Type::Fix(_, t_) => Ok(t_.sub_delay_fix(&fix_var)),
                    Type::GenericVar(_) => Ok(a),
                    _ => unreachable!(),
                }
            }
            Into(e) => {
                let t = self.infer(e, ctx.clone())?;
                let fix_var = self.fresh_type_var();
                let t_ret = t.sub_delay_fix(&fix_var);

                Ok(Type::Fix(fix_var, Box::new(t_ret)))
            }
            List(v) => {
                let mut types = Vec::new();
                for e in v {
                    types.push(self.infer(e, ctx.clone())?);
                }
                let t_ret = Type::GenericVar(self.fresh_type_var());

                let mut constraints = VecDeque::new();
                for t in &types {
                    constraints.push_back((t.clone(), t_ret.clone()));
                }
                let mut subs = unify(constraints);
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(Type::List(Box::new(t_ret)))
            }
            Tuple(v) => {
                let mut types = Vec::new();
                for e in v {
                    types.push(Box::new(self.infer(e, ctx.clone())?));
                }

                Ok(Type::Tuple(types))
            }
            Struct(id, v) => {
                let mut fields = Vec::new();
                for (f, e) in v {
                    fields.push((f, self.infer(e, ctx.clone())?));
                }

                let t_struct = match self.type_decs.clone().get(id) {
                    Some(t) => match t {
                        // Instantiate generic variables
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var());
                                t_ = t.sub_generic(&arg, &fresh_t);
                            }

                            t_
                        }
                        t => t.clone(),
                    },

                    None => return Err(TypeError::UserTypeNotFound(id.clone()).into()),
                };

                match &t_struct {
                    Type::Struct(map) => {
                        let mut constraints = VecDeque::new();
                        for (f, t) in fields {
                            let t_ = match map.get(f) {
                                Some(t) => *t.clone(),
                                None => {
                                    return Err(TypeError::StructFieldDoesNotExist(
                                        id.clone(),
                                        f.clone(),
                                    )
                                    .into())
                                }
                            };
                            constraints.push_back((t, t_));
                        }
                        let mut subs = unify(constraints);
                        self.substitutions.append(&mut subs);
                        ctx.apply_subs(&self.substitutions);

                        Ok(t_struct.clone())
                    }
                    _ => Err(TypeError::NotAStruct(id.clone()).into()),
                }
            }
            Variant(id, o) => {
                let t = match o {
                    Some(e) => Some(self.infer(&e, ctx.clone())?),
                    None => None
                };

                let t_enum = match self.variant_map.get(id) {
                    Some(enm) => match self.type_decs.clone().get(enm).unwrap() {
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var());
                                t_ = t.sub_generic(&arg, &fresh_t);
                            }

                            t_
                        }
                        t => t.clone(),

                    }
                    None => return Err(TypeError::EnumVariantDoesNotExist(id.clone()).into())
                };
                
                match &t_enum {
                    Type::Enum(map) => {
                        match (t, map.get(id).unwrap()) { 
                            (None, None) => Ok(t_enum),
                            (Some(t1), Some(t2)) => {
                                let mut subs = unify(VecDeque::from([(t1.clone(), *t2.clone())]));
                                self.substitutions.append(&mut subs);
                                ctx.apply_subs(&self.substitutions);

                                Ok(t_enum.clone())
                            }
                            _ => Err(TypeError::VariantFieldsDoNotMatch(id.clone()).into()),
                        }
                    }
                    _ => Err(TypeError::NotAnEnum(id.clone()).into()),
                }
            }
            Fn(var, e) => {
                let t1 = Type::GenericVar(self.fresh_type_var());
                let mut ctx = ctx.clone();
                ctx.push_var(var.clone(), t1.clone());

                let t2 = self.infer(e, ctx.clone())?;

                Ok(Type::Function(Box::new(ctx.get_var(var)?), Box::new(t2)))
            }
            Fix(alpha, e) => {
                let mut ctx = ctx.clone().stable()?;
                let t_ret = Type::GenericVar(self.fresh_type_var());
                ctx.push_var(alpha.clone(), Type::Stable(Box::new(Type::Delay(Box::new(t_ret.clone())))));

                let t = self.infer(e, ctx.clone())?;
                let mut subs = unify(VecDeque::from([(t.clone(), t_ret.clone())]));
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t_ret)
            }
            If(e1, e2, e3) => {
                let t1 = self.infer(e1, ctx.clone())?;
                let t2 = self.infer(e2, ctx.clone())?;
                let t3 = self.infer(e3, ctx.clone())?;

                let mut subs = unify(VecDeque::from([
                        (t1.clone(), Type::Bool),
                        (t2.clone(), t3.clone())
                ]));
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t3)
            }
            Seq(e1, e2) => match *e1.clone() {
                Let(id, e) => {
                    let t_e = self.infer(&e, ctx.clone())?;
                    let mut t_e_free_vars = t_e.get_free_vars();
                    let ctx_free_vars = ctx.get_free_vars();

                    for var in ctx_free_vars {
                        t_e_free_vars.remove(&var);

                    }

                    let mut ctx = ctx.clone();
                    ctx.push_var(id, Type::Generic(t_e_free_vars.into_iter().collect(), Box::new(t_e.clone())));

                    Ok(self.infer(e2, ctx)?)
                }
                _ => todo!()
            }
            App(e1, e2) => {
                let t1 = self.infer(e1, ctx.clone())?;
                let t2 = self.infer(e2, ctx.clone())?;
                let t3 = Type::GenericVar(self.fresh_type_var());

                let mut subs = unify(VecDeque::from([(
                    t1,
                    Type::Function(Box::new(t2), Box::new(t3.clone())),
                )]));
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t3)
            }
            ProjTuple(e, i) => {
                let t = self.infer(e, ctx.clone())?;
                match t {
                    Type::Tuple(v) => Ok(*v.get(*i as usize).unwrap().clone()),
                    _ => todo!()
                }
            }
            ProjStruct(e, f) => {
                let t = self.infer(e, ctx.clone())?;
                match t {
                    Type::Struct(map) => match map.get(f) {
                        Some(t) => Ok(*t.clone()),
                        None => return Err(TypeError::StructFieldDoesNotExist("?".to_string(), f.clone()).into()),
                    }
                    _ => todo!()
                }
            }
            Match(e, v) => {
                let t_e = self.infer(e, ctx.clone())?;
                let t_ret = Type::GenericVar(self.fresh_type_var());

                let mut constraints = VecDeque::new();
                for (p, e_p) in v {
                    // Add p's variables into the context
                    let t_e_p = self.infer(e_p, ctx.clone())?;
                    constraints.push_back((t_e_p.clone(), t_ret.clone()));

                    // t_e needs to match p's type
                    // e_p's types all need to match
                    
                }
                todo!()
            }
            Var(var) => match ctx.clone().get_var(var) {
                Ok(t) => match t {
                    // Instantiate generic variables
                    Type::Generic(scheme, t) => {
                        let mut t_ = *t.clone();
                        for arg in scheme {
                            let fresh_t = Type::GenericVar(self.fresh_type_var());
                            t_ = t.sub_generic(&arg, &fresh_t);
                        }

                        Ok(t_)
                    }
                    t => Ok(t),
                },
                // Check that it's not a global variable
                Err(e) => match self.value_decs.clone().get(var) {
                    Some(t) => match t {
                        // Instantiate generic variables
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var());
                                t_ = t.sub_generic(&arg, &fresh_t);
                            }

                            Ok(t_)
                        }
                        t => Ok(t.clone()),
                    },
                    None => Err(e),
                },
            },
            Let(..) => Err(InvalidExprError::IllegalLetExpr.into()),
            _ => todo!(),
        }
    }

    fn infer_binop(&mut self, t1: Type, op: BOpcode, t2: Type) -> Result<Type> {
        use BOpcode::*;
        use Type::*;
        use TypeError::ImproperType;
        match (t1, op, t2) {
            (GenericVar(v1), Add, GenericVar(_)) => Ok(GenericVar(v1)),
            (Int, Add, Int) | (Int, Add, GenericVar(_)) | (GenericVar(_), Add, Int) => Ok(Int),
            (Float, Add, Float) | (Float, Add, GenericVar(_)) | (GenericVar(_), Add, Float) => {
                Ok(Float)
            }
            (t1, Add, t2) => Err(ImproperType(
                "Float or Int".to_string(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(v1), Sub, GenericVar(_)) => Ok(GenericVar(v1)),
            (Int, Sub, Int) | (Int, Sub, GenericVar(_)) | (GenericVar(_), Sub, Int) => Ok(Int),
            (Float, Sub, Float) | (Float, Sub, GenericVar(_)) | (GenericVar(_), Sub, Float) => {
                Ok(Float)
            }
            (t1, Sub, t2) => Err(ImproperType(
                "Float or Int".to_string(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(v1), Mul, GenericVar(_)) => Ok(GenericVar(v1)),
            (Int, Mul, Int) | (Int, Mul, GenericVar(_)) | (GenericVar(_), Mul, Int) => Ok(Int),
            (Float, Mul, Float) | (Float, Mul, GenericVar(_)) | (GenericVar(_), Mul, Float) => {
                Ok(Float)
            }
            (t1, Mul, t2) => Err(ImproperType(
                "Float or Int".to_string(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(v1), Div, GenericVar(_)) => Ok(GenericVar(v1)),
            (Int, Div, Int) | (Int, Div, GenericVar(_)) | (GenericVar(_), Div, Int) => Ok(Int),
            (Float, Div, Float) | (Float, Div, GenericVar(_)) | (GenericVar(_), Div, Float) => {
                Ok(Float)
            }
            (t1, Div, t2) => Err(ImproperType(
                "Float or Int".to_string(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(_), Mod, GenericVar(_))
            | (Int, Mod, Int)
            | (Int, Mod, GenericVar(_))
            | (GenericVar(_), Mod, Int) => Ok(Int),
            (t1, Mod, t2) => Err(ImproperType(
                "Float or Int".to_string(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(v1), Cons, GenericVar(_)) => Ok(List(Box::new(GenericVar(v1)))),
            (t1, Cons, List(t2)) => {
                if t1 == *t2 {
                    Ok(List(Box::new(t1)))
                } else {
                    Err(ImproperType(
                        format!("{:?} and [{:?}]", t1, t1),
                        format!("{:?} and {:?}", t1, List(t2)),
                    )
                    .into())
                }
            }
            (t, Cons, GenericVar(_)) => Ok(List(Box::new(t))),
            (t1, Cons, t2) => Err(ImproperType(
                format!("{:?} and [{:?}]", t1, t1),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(_), Eq, GenericVar(_))
            | (_, Eq, GenericVar(_))
            | (GenericVar(_), Eq, _) => Ok(Bool),
            (t1, Eq, t2) => {
                Err(ImproperType("same types".to_string(), format!("{:?} and {:?}", t1, t2)).into())
            }

            (GenericVar(_), Lt, GenericVar(_))
            | (Int, Lt, Int)
            | (Int, Lt, GenericVar(_))
            | (GenericVar(_), Lt, Int)
            | (Float, Lt, Float)
            | (Float, Lt, GenericVar(_))
            | (GenericVar(_), Lt, Float) => Ok(Bool),
            (t1, Lt, t2) => Err(ImproperType(
                "Float or Int".to_string(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(_), Gt, GenericVar(_))
            | (Int, Gt, Int)
            | (Int, Gt, GenericVar(_))
            | (GenericVar(_), Gt, Int)
            | (Float, Gt, Float)
            | (Float, Gt, GenericVar(_))
            | (GenericVar(_), Gt, Float) => Ok(Bool),
            (t1, Gt, t2) => Err(ImproperType(
                "Float or Int".to_string(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),

            (GenericVar(_), And, GenericVar(_))
            | (Bool, And, Bool)
            | (Bool, And, GenericVar(_))
            | (GenericVar(_), And, Bool) => Ok(Bool),
            (t1, And, t2) => {
                Err(ImproperType("two Bools".to_string(), format!("{:?} and {:?}", t1, t2)).into())
            }

            (GenericVar(_), Or, GenericVar(_))
            | (Bool, Or, Bool)
            | (Bool, Or, GenericVar(_))
            | (GenericVar(_), Or, Bool) => Ok(Bool),
            (t1, Or, t2) => {
                Err(ImproperType("two Bools".to_string(), format!("{:?} and {:?}", t1, t2)).into())
            }
        }
    }

    fn fresh_type_var(&mut self) -> String {
        let x = self.fresh_type_var;
        self.fresh_type_var += 1;

        format!("__t{}", x)
    }
}

fn unify(mut constraints: VecDeque<(Type, Type)>) -> Vec<(String, Type)> {
    use Type::*;
    match &constraints.pop_front() {
        None => Vec::new(),
        Some((t1, t2)) => {
            if t1 == t2 {
                unify(constraints)
            } else {
                match (t1, t2) {
                    (GenericVar(alpha), t2) => {
                        sub_constraints(&mut constraints, &GenericVar(alpha.clone()), &t2);
                        let mut subs = unify(constraints);

                        subs.push((alpha.clone(), t2.clone()));
                        subs
                    }
                    (t1, GenericVar(alpha)) => {
                        sub_constraints(&mut constraints, &GenericVar(alpha.clone()), &t1);
                        let mut subs = vec![(alpha.clone(), t2.clone())];
                        subs.append(&mut unify(constraints));

                        subs
                    }
                    (Function(t1, t2), Function(t3, t4)) => {
                        constraints.push_front((*t1.clone(), *t3.clone()));
                        constraints.push_front((*t2.clone(), *t4.clone()));

                        unify(constraints)
                    }
                    (List(t1), List(t2)) => {
                        constraints.push_front((*t1.clone(), *t2.clone()));

                        unify(constraints)
                    }
                    (Delay(t1), Delay(t2)) => {
                        constraints.push_front((*t1.clone(), *t2.clone()));

                        unify(constraints)
                    }
                    (Stable(t1), Stable(t2)) => {
                        constraints.push_front((*t1.clone(), *t2.clone()));

                        unify(constraints)
                    }
                    (Fix(_alpha, t1), Fix(_beta, t2)) => {
                        constraints.push_front((*t1.clone(), *t2.clone()));

                        unify(constraints)
                    }
                    (FixVar(_alpha), FixVar(_beta)) => unify(constraints),
                    _ => todo!(),
                }
            }
        }
    }
}

fn sub_constraints(constraints: &mut VecDeque<(Type, Type)>, var: &Type, val: &Type) {
    for (t1, t2) in constraints {
        if t1 == var {
            *t1 = val.clone()
        }
        if t2 == var {
            *t2 = val.clone()
        }
    }
}
