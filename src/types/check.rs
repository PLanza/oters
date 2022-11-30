use std::collections::{HashMap, VecDeque};

use super::{Type, TypeContext, TypeError};
use crate::exprs::{BOpcode, Expr, InvalidExprError, InvalidPatternError, UOpcode, VarContext};
use crate::parser::ast::{PExpr, Pattern, Program};

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
                    let mut t = self.infer(&e, VarContext::new())?;

                    // Unify the substitutions
                    self.unify_subs()?;

                    // Apply the substitutions
                    t = t.apply_subs(&self.substitutions);

                    // Reduce Fix types
                    t = t.sub_delay_fix(&"".to_owned());

                    // Convert free variables to generics
                    let free_vars = t.get_free_vars();

                    let t = if !free_vars.is_empty() {
                        Type::Generic(free_vars.into_iter().collect(), Box::new(t))
                    } else {
                        t
                    };

                    // Make sure the resulting type is well formed
                    t.well_formed(TypeContext::new())?;

                    // Add it to the map of value declarations
                    self.value_decs.insert(id.clone(), t);
                }
                _ => return Err(InvalidExprError::InvalidTopLevelExpr(expr.head_string()).into()),
            }
        }

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
                    ]))?,
                    BOpcode::Eq | BOpcode::Lt | BOpcode::Gt => {
                        unify(VecDeque::from([(t1.clone(), t2.clone())]))?
                    }
                    _ => unify(VecDeque::from([
                        (t1.clone(), t3.clone()),
                        (t2.clone(), t3.clone()),
                        (t1.clone(), t2.clone()),
                    ]))?,
                };
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t3)
            }
            UnOp(UOpcode::Neg, e) => {
                let t = self.infer(e, ctx.clone())?;
                let mut subs = unify(VecDeque::from([(t.clone(), Type::Int)]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(Type::Int)
            }
            UnOp(UOpcode::Not, e) => {
                let t = self.infer(e, ctx.clone())?;
                let mut subs = unify(VecDeque::from([(t.clone(), Type::Bool)]))?;
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
                )]))?;
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
                )]))?;
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
                )]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                match t {
                    Type::Fix(_, t_) => Ok(t_.sub_delay_fix(&fix_var)),
                    Type::GenericVar(_) => Ok(a),
                    _ => Err(TypeError::ImproperType("Fix α.t".to_string(), format!("{:?}", t)).into()),
                }
            }
            Into(e) => {
                let t = self.infer(e, ctx.clone())?;
                let fix_var = self.fresh_type_var();

                Ok(Type::Fix(fix_var, Box::new(t)))
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
                let mut subs = unify(constraints)?;
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
                                t_ = t_.sub_generic(&arg, &fresh_t);
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
                        let mut subs = unify(constraints)?;
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
                    None => None,
                };

                let t_enum = match self.variant_map.get(id) {
                    Some(enm) => match self.type_decs.clone().get(enm).unwrap() {
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var());
                                t_ = t_.sub_generic(&arg, &fresh_t);
                            }

                            t_
                        }
                        t => t.clone(),
                    },
                    None => return Err(TypeError::EnumVariantDoesNotExist(id.clone()).into()),
                };

                match &t_enum {
                    Type::Enum(map) => match (t, map.get(id).unwrap()) {
                        (None, None) => Ok(t_enum),
                        (Some(t1), Some(t2)) => {
                            let mut subs = unify(VecDeque::from([(t1.clone(), *t2.clone())]))?;
                            self.substitutions.append(&mut subs);
                            ctx.apply_subs(&self.substitutions);

                            Ok(t_enum.clone())
                        }
                        _ => Err(TypeError::VariantFieldsDoNotMatch(id.clone()).into()),
                    },
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
                ctx.push_var(
                    alpha.clone(),
                    Type::Stable(Box::new(Type::Delay(Box::new(t_ret.clone())))),
                );

                let t = self.infer(e, ctx.clone())?;
                let mut subs = unify(VecDeque::from([(t_ret.clone(), t.clone())]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t)
            }
            If(e1, e2, e3) => {
                let t1 = self.infer(e1, ctx.clone())?;
                let t2 = self.infer(e2, ctx.clone())?;
                let t3 = self.infer(e3, ctx.clone())?;

                let mut subs = unify(VecDeque::from([
                    (t1.clone(), Type::Bool),
                    (t2.clone(), t3.clone()),
                ]))?;
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
                    ctx.push_var(
                        id,
                        Type::Generic(t_e_free_vars.into_iter().collect(), Box::new(t_e.clone())),
                    );

                    Ok(self.infer(e2, ctx)?)
                }
                _ => {
                    let t = self.infer(&e, ctx.clone())?;

                    let mut subs = unify(VecDeque::from([(t.clone(), Type::Unit)]))?;
                    self.substitutions.append(&mut subs);
                    ctx.apply_subs(&self.substitutions);

                    Ok(self.infer(e2, ctx)?)
                }
            },
            App(e1, e2) => {
                let t1 = self.infer(e1, ctx.clone())?;
                let t2 = self.infer(e2, ctx.clone())?;
                let t3 = Type::GenericVar(self.fresh_type_var());

                let mut subs = unify(VecDeque::from([(
                    t1.clone(),
                    Type::Function(Box::new(t2.clone()), Box::new(t3.clone())),
                )]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t3)
            }
            ProjStruct(e, f) => {
                let t = self.infer(e, ctx.clone())?;
                let field_type = Type::GenericVar(self.fresh_type_var());
                let map = HashMap::from([(f.clone(), Box::new(field_type))]);

                let mut subs = unify(VecDeque::from([(t.clone(), Type::Struct(map))]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(t)
            }
            Match(e, v) => {
                let t_e = self.infer(e, ctx.clone())?;
                let t_ret = Type::GenericVar(self.fresh_type_var());

                let mut constraints = VecDeque::new();
                for (p, e_p) in v {
                    // Add p's variables into the context
                    let (t_p, p_vars) = self.check_pattern(p.clone())?;
                    constraints.push_back((t_e.clone(), t_p.clone()));

                    let mut p_ctx = ctx.clone();
                    for (var, t_var) in p_vars {
                        p_ctx.push_var(var, t_var);
                    }
                    p_ctx.apply_subs(&self.substitutions);

                    let t_e_p = self.infer(e_p, p_ctx.clone())?;
                    constraints.push_back((t_e_p.clone(), t_ret.clone()));
                }

                let mut subs = unify(constraints)?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&subs);

                Ok(t_ret)
            }
            Var(var) => match ctx.clone().get_var(var) {
                Ok(t) => match t {
                    // Instantiate generic variables
                    Type::Generic(scheme, t) => {
                        let mut t_ = *t.clone();
                        for arg in scheme {
                            let fresh_t = Type::GenericVar(self.fresh_type_var());
                            t_ = t_.sub_generic(&arg, &fresh_t);
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
                                t_ = t_.sub_generic(&arg, &fresh_t);
                            }

                            Ok(t_)
                        }
                        t => Ok(t.clone()),
                    },
                    None => Err(e),
                },
            },
            Let(..) => Err(InvalidExprError::IllegalLetExpr.into()),
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

    pub fn check_pattern(&mut self, p: Pattern) -> Result<(Type, HashMap<String, Type>)> {
        use Pattern::*;
        match p {
            Underscore => Ok((Type::GenericVar(self.fresh_type_var()), HashMap::new())),
            True => Ok((Type::Bool, HashMap::new())),
            False => Ok((Type::Bool, HashMap::new())),
            Int(_) => Ok((Type::Int, HashMap::new())),
            Float(_) => Ok((Type::Float, HashMap::new())),
            String(_) => Ok((Type::String, HashMap::new())),
            Unit => Ok((Type::Unit, HashMap::new())),
            Tuple(v) => {
                let mut vars = HashMap::new();
                let mut types = Vec::new();
                for p in v {
                    let (t, p_vars) = self.check_pattern(*p.clone())?;
                    types.push(Box::new(t));
                    for (var, t_var) in p_vars {
                        match vars.insert(var.clone(), t_var) {
                            None => (),
                            Some(_) => {
                                return Err(InvalidPatternError::SimultaneousPatternBinding(
                                    var.clone(),
                                    format!("{:?}", p.clone()),
                                )
                                .into())
                            }
                        }
                    }
                }
                Ok((Type::Tuple(types), vars))
            }
            List(v) => {
                let mut vars = HashMap::new();
                let mut t_list = None;
                for p in v {
                    let (t, p_vars) = self.check_pattern(*p.clone())?;
                    // Check all sub patterns correspond to the same type
                    t_list = match t_list {
                        None => Some(t),
                        Some(t_) => {
                            if t == t_ {
                                Some(t_)
                            } else {
                                return Err(InvalidPatternError::InvalidListPattern.into());
                            }
                        }
                    };

                    // Check for no repeating variable names
                    for (var, t_var) in p_vars {
                        match vars.insert(var.clone(), t_var) {
                            None => (),
                            Some(_) => {
                                return Err(InvalidPatternError::SimultaneousPatternBinding(
                                    var.clone(),
                                    format!("{:?}", p.clone()),
                                )
                                .into())
                            }
                        }
                    }
                }

                // If empty list then return generic list
                match t_list {
                    None => Ok((
                        Type::List(Box::new(Type::GenericVar(self.fresh_type_var()))),
                        vars,
                    )),
                    Some(t) => Ok((Type::List(Box::new(t)), vars)),
                }
            }
            Variant(c, o) => {
                let (t, vars) = match o {
                    Some(p) => {
                        let (t_, p_vars) = self.check_pattern(*p.clone())?;
                        (Some(t_), p_vars)
                    }
                    None => (None, HashMap::new()),
                };

                let t_enum = match self.variant_map.get(&c) {
                    Some(enm) => match self.type_decs.clone().get(enm).unwrap() {
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var());
                                t_ = t_.sub_generic(&arg, &fresh_t);
                            }

                            t_
                        }
                        t => t.clone(),
                    },
                    None => return Err(TypeError::EnumVariantDoesNotExist(c.clone()).into()),
                };

                match &t_enum {
                    Type::Enum(map) => match (t, map.get(&c).unwrap()) {
                        (None, None) => Ok((t_enum, vars)),
                        (Some(t1), Some(t2)) => {
                            let mut subs = unify(VecDeque::from([(t1.clone(), *t2.clone())]))?;
                            self.substitutions.append(&mut subs);

                            Ok((t_enum.clone(), vars))
                        }
                        _ => Err(TypeError::VariantFieldsDoNotMatch(c.clone()).into()),
                    },
                    _ => Err(TypeError::NotAnEnum(c.clone()).into()),
                }
            }
            Struct(id, v) => {
                let mut fields = Vec::new();
                let mut vars = HashMap::new();
                for (f, p) in v {
                    let (t, p_vars) = match &p {
                        None => break,
                        Some(p) => self.check_pattern(*p.clone())?,
                    };

                    fields.push((f, t));
                    for (var, t_var) in p_vars {
                        match vars.insert(var.clone(), t_var) {
                            None => (),
                            Some(_) => {
                                return Err(InvalidPatternError::SimultaneousPatternBinding(
                                    var.clone(),
                                    format!("{:?}", p.clone()),
                                )
                                .into())
                            }
                        }
                    }
                }

                let t_struct = match self.type_decs.clone().get(&id) {
                    Some(t) => match t {
                        // Instantiate generic variables
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var());
                                t_ = t_.sub_generic(&arg, &fresh_t);
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
                            let t_ = match map.get(&f) {
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
                        let mut subs = unify(constraints)?;
                        self.substitutions.append(&mut subs);

                        Ok((t_struct.clone(), vars))
                    }
                    _ => Err(TypeError::NotAStruct(id.clone()).into()),
                }
            }
            Cons(p1, p2) => {
                let (t1, mut vars1) = self.check_pattern(*p1.clone())?;
                let (t2, vars2) = self.check_pattern(*p2.clone())?;
                for (var, t_var) in vars2 {
                    match vars1.insert(var.clone(), t_var) {
                        None => (),
                        Some(_) => {
                            return Err(InvalidPatternError::SimultaneousPatternBinding(
                                var.clone(),
                                format!("{:?}", Cons(p1.clone(), p2.clone())),
                            )
                            .into())
                        }
                    }
                }

                let t_ret = match (t1.clone(), t2.clone()) {
                    (Type::GenericVar(v1), Type::GenericVar(_)) => {
                        Type::List(Box::new(Type::GenericVar(v1)))
                    }
                    (t1, Type::List(t2)) => {
                        if t1 == *t2 {
                            Type::List(Box::new(t1))
                        } else {
                            return Err(TypeError::ImproperType(
                                format!("{:?} and [{:?}]", t1, t1),
                                format!("{:?} and {:?}", t1, Type::List(t2)),
                            )
                            .into());
                        }
                    }
                    (t, Type::GenericVar(_)) => Type::List(Box::new(t)),
                    (t1, t2) => {
                        return Err(TypeError::ImproperType(
                            format!("{:?} and [{:?}]", t1, t1),
                            format!("{:?} and {:?}", t1, t2),
                        )
                        .into())
                    }
                };

                let mut subs = unify(VecDeque::from([
                    (t2.clone(), Type::List(Box::new(t1.clone()))),
                    (t2.clone(), t_ret.clone()),
                ]))?;
                self.substitutions.append(&mut subs);

                Ok((t_ret, vars1))
            }
            Stream(x, xs) => {
                let (t, mut vars) = self.check_pattern(*x.clone())?;
                match *xs {
                    Var(xs) => {
                        let fix_var = self.fresh_type_var();
                        let t_ret = Type::Fix(
                            fix_var.clone(),
                            Box::new(Type::Tuple(vec![
                                Box::new(t.clone()),
                                Box::new(Type::FixVar(fix_var)),
                            ])),
                        );
                        match vars.insert(xs.clone(), Type::Delay(Box::new(t_ret.clone()))) {
                            None => (),
                            Some(_) => {
                                return Err(InvalidPatternError::SimultaneousPatternBinding(
                                    xs.clone(),
                                    format!("{:?} << {}", x.clone(), xs.clone()),
                                )
                                .into())
                            }
                        }
                        Ok((t_ret, vars))
                    }
                    _ => Err(InvalidPatternError::InvalidStreamPattern.into()),
                }
            }
            Or(p1, p2) => {
                let (t1, mut vars1) = self.check_pattern(*p1.clone())?;
                let (t2, vars2) = self.check_pattern(*p2.clone())?;
                for (var, t_var) in vars2 {
                    match vars1.insert(var.clone(), t_var) {
                        None => (),
                        Some(_) => {
                            return Err(InvalidPatternError::SimultaneousPatternBinding(
                                var.clone(),
                                format!("{:?}", Cons(p1.clone(), p2.clone())),
                            )
                            .into())
                        }
                    }
                }

                let mut subs = unify(VecDeque::from([(t1.clone(), t2.clone())]))?;
                self.substitutions.append(&mut subs);

                Ok((t2, vars1))
            }
            Delay(p_) => {
                let (t, vars) = self.check_pattern(*p_)?;
                Ok((Type::Delay(Box::new(t)), vars))
            }
            Stable(p_) => {
                let (t, vars) = self.check_pattern(*p_)?;
                Ok((Type::Stable(Box::new(t)), vars))
            }
            Var(var) => {
                let t = Type::GenericVar(self.fresh_type_var());
                Ok((t.clone(), HashMap::from([(var, t)])))
            }
        }
    }

    fn fresh_type_var(&mut self) -> String {
        let x = self.fresh_type_var;
        self.fresh_type_var += 1;

        format!("__t{}", x)
    }

    fn unify_subs(&mut self) -> Result<()>{
        let mut sub_map = HashMap::new();
        let mut i = 0;
        while i < self.substitutions.len() {
            let (x1, t) = self.substitutions[i].clone();

            match sub_map.get_mut(&x1) {
                None => { sub_map.insert(x1.clone(), vec![i]); },
                Some(indices) => {
                    let mut constraints = VecDeque::new();
                    for index in indices.clone() {
                        constraints.push_back((t.clone(), self.substitutions[index].1.clone()));
                    }
                    let mut subs = unify(constraints)?;
                    self.substitutions.append(&mut subs);

                    (*indices).push(i);
                },
            }
            i += 1;
        }
        Ok(())
    }
}

fn unify(mut constraints: VecDeque<(Type, Type)>) -> Result<Vec<(String, Type)>> {
    use Type::*;
    match &constraints.pop_front() {
        None => Ok(Vec::new()),
        Some((t1, t2)) => {
            if t1 == t2 {
                unify(constraints)
            } else {
                match (t1, t2) {
                    (GenericVar(alpha), t2) => {
                        sub_constraints(&mut constraints, &GenericVar(alpha.clone()), &t2);
                        let mut subs = unify(constraints)?;

                        subs.push((alpha.clone(), t2.clone()));
                        Ok(subs)
                    }
                    (t1, GenericVar(alpha)) => {
                        sub_constraints(&mut constraints, &GenericVar(alpha.clone()), &t1);
                        let mut subs = unify(constraints)?;

                        subs.push((alpha.clone(), t1.clone()));
                        Ok(subs)
                    }
                    (Tuple(v1), Tuple(v2)) => {
                        if v1.len() != v2.len() {
                            Err(
                                TypeError::ImproperType(format!("{:?}", t1), format!("{:?}", t2))
                                    .into(),
                            )
                        } else {
                            for (t1, t2) in v1.iter().zip(v2) {
                                constraints.push_front((*t1.clone(), *t2.clone()));
                            }
                            unify(constraints)
                        }
                    }
                    (List(t1), List(t2)) => {
                        constraints.push_front((*t1.clone(), *t2.clone()));

                        unify(constraints)
                    }
                    (Function(t1, t2), Function(t3, t4)) => {
                        constraints.push_front((*t1.clone(), *t3.clone()));
                        constraints.push_front((*t2.clone(), *t4.clone()));

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
                    (Generic(scheme1, t1), Generic(scheme2, t2)) => {
                        let mut t1 = *t1.clone();
                        for arg in scheme1 {
                            // Probably wrong
                            let fresh_t = Type::GenericVar(arg.to_string());
                            t1 = t1.sub_generic(&arg, &fresh_t);
                        }

                        let mut t2 = *t2.clone();
                        for arg in scheme2 {
                            // Probably wrong
                            let fresh_t = Type::GenericVar(arg.to_string());
                            t2 = t2.sub_generic(&arg, &fresh_t);
                        }

                        constraints.push_front((t1, t2));

                        unify(constraints)
                    }
                    (Struct(map1), Struct(map2)) => {
                        let (smaller, bigger) = if map1.len() < map2.len() {
                            (map1, map2)
                        } else {
                            (map2, map1)
                        };
                        for (field, t1) in smaller {
                            match bigger.get(field) {
                                None => {
                                    return Err(TypeError::StructFieldDoesNotExist(
                                        format!("{:?}", Struct(bigger.clone())),
                                        field.clone(),
                                    )
                                    .into())
                                }
                                Some(t2) => constraints.push_front((*t1.clone(), *t2.clone())),
                            }
                        }
                        unify(constraints)
                    }
                    (Enum(map1), Enum(map2)) => {
                        for (c1, o1) in map1 {
                            match map2.get(c1) {
                                None => return Err(TypeError::EnumsDoNotMatch.into()),
                                Some(o2) => match (o1, o2) {
                                    (Some(t1), Some(t2)) => {
                                        constraints.push_front((*t1.clone(), *t2.clone()))
                                    }
                                    (None, None) => (),
                                    _ => return Err(TypeError::EnumsDoNotMatch.into()),
                                },
                            }
                        }
                        unify(constraints)
                    }
                    _ => Err(
                        TypeError::ImproperType(format!("{:?}", t1), format!("{:?}", t2)).into(),
                    ),
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
