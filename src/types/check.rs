use std::collections::HashMap;

use super::{Type, TypeContext, TypeError};
use crate::parser::ast::{PExpr, Program};
use crate::exprs::{VarContext, InvalidExprError, Expr, BOpcode, UOpcode};

use anyhow::Result;

pub struct ProgramChecker {
    // These represent the global top-level declarations
    value_decs: HashMap<String, Type>, // Maps top-level values (i.e. `let` exprs) to their type
    type_decs: HashMap<String, Type>,  // Maps `type`s, `enum`s and `struct`s to their definitions
    variant_map: HashMap<String, String>, // Maps variants to their corresponding `enum`
}

impl ProgramChecker {
    pub fn new() -> Self {
        ProgramChecker {
            value_decs: HashMap::new(),
            type_decs: HashMap::new(),
            variant_map: HashMap::new(),
        }
    }

    pub fn type_check_program(&mut self, program: &Program) -> Result<()> {
        for expr in program {
            let mut type_context = TypeContext::new();

            match expr.as_ref() {
                // Type Aliases
                PExpr::TypeDef(t_id, params, t) => {
                    // The type being aliased
                    let t = if params.is_empty() {
                        t.clone().to_type(type_context.clone(), &self.type_decs)?
                    } else {
                        // Extend the context with current params to disambiguate TEUsers
                        type_context.extend(&mut params.clone());

                        // The type being aliased
                        Type::Generic(
                            params.clone(),
                            Box::new(t.clone().to_type(type_context.clone(), &self.type_decs)?),
                        )
                    };

                    // Check that the type is well formed
                    type_context.well_formed(&t, &self.type_decs)?;

                    // Add it to the type declarations
                    self.type_decs.insert(t_id.clone(), t);
                }
                PExpr::EnumDef(id, params, ts) => {
                    // Extend type_context to convert generic args safely
                    if !params.is_empty() {
                        type_context.extend(&mut params.clone());
                    }

                    let mut variants = HashMap::new();

                    // Convert TypeExprs to Types
                    for (s, o) in ts {
                        let v_type = match o {
                            None => None,
                            Some(t_expr) => Some(Box::new(
                                t_expr
                                    .clone()
                                    .to_type(type_context.clone(), &self.type_decs)?,
                            )),
                        };
                        variants.insert(s.clone(), v_type);
                    }

                    // Create the Enum type
                    let t = if params.is_empty() {
                        Type::Enum(variants)
                    } else {
                        Type::Generic(params.clone(), Box::new(Type::Enum(variants)))
                    };

                    // Check the enum is well formed
                    type_context.well_formed(&t, &self.type_decs)?;

                    // Add enum to the type declarations
                    self.type_decs.insert(id.clone(), t);

                    // Add variants to map
                    for (s, _) in ts {
                        self.variant_map.insert(s.clone(), id.clone());
                    }
                }
                PExpr::StructDef(id, params, ts) => {
                    // Extend type_context to convert generic args safely
                    if !params.is_empty() {
                        type_context.extend(&mut params.clone());
                    }

                    let mut fields = HashMap::new();

                    // Convert TypeExprs to Types
                    for (s, t) in ts {
                        fields.insert(
                            s.clone(),
                            Box::new(t.clone().to_type(type_context.clone(), &self.type_decs)?),
                        );
                    }

                    // Create the Struct type
                    let t = if params.is_empty() {
                        Type::Struct(fields)
                    } else {
                        Type::Generic(params.clone(), Box::new(Type::Struct(fields)))
                    };

                    // Check the struct is well formed
                    type_context.well_formed(&t, &self.type_decs)?;

                    // Add struct to the type declarations
                    self.type_decs.insert(id.clone(), t);
                }
                PExpr::Let(id, params, expr) => {
                    // Extend type_context to safely type check the expression
                    if !params.is_empty() {
                        type_context.extend(&mut params.clone());
                    }

                    // Translate from parsed expression to language expressions 
                    let e = match expr.clone().to_expr(&type_context, &self.type_decs)? {
                        // Recursive functions are translated to fix points to ensure guarded recursion
                        Expr::Fn(ts, fn_e) => {
                            let (is_rec, rec_e) = fn_e.clone().substitute(&id, 
                                &Expr::UnOp(UOpcode::Adv, Box::new(Expr::UnOp(UOpcode::Unbox, Box::new(
                                                Expr::Var(id.clone()))))));
                            if is_rec {
                                Expr::Let(
                                    id.clone(),
                                    Box::new(Expr::Fix(id.clone(), Box::new(Expr::Fn(ts, Box::new(rec_e))))
                                    )) 
                            } else {
                                Expr::Let(
                                    id.clone(),
                                    Box::new(Expr::Fn(ts, fn_e))
                                )
                            }
                        }
                        e => Expr::Let(id.clone(), Box::new(e))
                    };


                    // Type check the expression
                    let t = self.type_check_expr(&e, type_context.clone(), VarContext::new())?;
                    let t = if params.is_empty() {
                        t
                    } else {
                        Type::Generic(params.clone(), Box::new(t))
                    };

                    // Make sure the resulting type is well formed
                    type_context.well_formed(&t, &self.type_decs)?;

                    // Add it to the map of value declarations
                    self.value_decs.insert(id.clone(), t);
                }
                _ => return Err(InvalidExprError::InvalidTopLevelExpr(expr.head_string()).into()),
            }
        }

        println!("{:?}", self.type_decs);
        Ok(())
    }

    fn type_check_expr(
        &self,
        expr: &Expr,
        t_context: TypeContext,
        v_context: VarContext,
    ) -> Result<Type> {
        match expr {
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::Int(_) => Ok(Type::Int),
            Expr::Float(_) => Ok(Type::Float),
            Expr::String(_) => Ok(Type::String),
            Expr::Unit => Ok(Type::Unit),
            Expr::BinOp(e1, op, e2) => {
                let t1 = self.type_check_expr(e1, t_context.clone(), v_context.clone())?;
                let t2 = self.type_check_expr(e2, t_context, v_context)?;
                self.type_check_bop(t1, t2, *op)
            }
            _ => todo!(),
        }
    }

    fn type_check_bop(&self, type_1: Type, type_2: Type, op: BOpcode) -> Result<Type> {
        use BOpcode::*;
        match (type_1, op, type_2) {
            (Type::Int, Mul, Type::Int)
            | (Type::Int, Div, Type::Int)
            | (Type::Int, Add, Type::Int)
            | (Type::Int, Sub, Type::Int)
            | (Type::Int, Mod, Type::Int) => Ok(Type::Int),
            (Type::Float, Mul, Type::Float)
            | (Type::Float, Div, Type::Float)
            | (Type::Float, Add, Type::Float)
            | (Type::Float, Sub, Type::Float)
            | (Type::Float, Mod, Type::Float) => Ok(Type::Float),
            (t1, Mul, t2) | (t1, Div, t2) | (t1, Add, t2) | (t1, Sub, t2) | (t1, Mod, t2) => Err(
                TypeError::ImproperType("Int or Float".into(), format!("{:?} and {:?}", t1, t2))
                    .into(),
            ),
            (Type::Int, Eq, Type::Int)
            | (Type::Int, Lt, Type::Int)
            | (Type::Int, Gt, Type::Int) => Ok(Type::Bool),
            (Type::Float, Eq, Type::Float)
            | (Type::Float, Lt, Type::Float)
            | (Type::Float, Gt, Type::Float) => Ok(Type::Bool),
            (t1, Eq, t2) | (t1, Lt, t2) | (t1, Gt, t2) => Err(TypeError::ImproperType(
                "Int or Float".into(),
                format!("{:?} and {:?}", t1, t2),
            )
            .into()),
            (Type::Bool, Or, Type::Bool) | (Type::Bool, And, Type::Bool) => Ok(Type::Bool),
            (t1, Or, t2) | (t1, And, t2) => {
                Err(TypeError::ImproperType("Bool".into(), format!("{:?} and {:?}", t1, t2)).into())
            }
            (t, Cons, Type::List(list_t)) => {
                if t == *list_t {
                    Ok(Type::List(Box::new(t)))
                } else {
                    Err(TypeError::ImproperType(format!("{:?}", list_t), format!("{:?}", t)).into())
                }
            }
            (t1, Cons, t2) => {
                Err(TypeError::ImproperType(format!("{:?} List", t1), format!("{:?}", t2)).into())
            }

        }
    }
}
