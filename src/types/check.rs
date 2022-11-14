use std::collections::HashMap;

use super::errors::*;
use super::{Type, TypeContext};
use crate::parser::ast::{Expr, Program};

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
                Expr::TypeDef(t_id, params, t) => {
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
                Expr::EnumDef(id, params, ts) => {
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
                },
                Expr::StructDef(id, params, ts) => {
                    // Extend type_context to convert generic args safely
                    if !params.is_empty() {
                        type_context.extend(&mut params.clone());
                    }

                    let mut fields = HashMap::new();

                    // Convert TypeExprs to Types
                    for (s, t) in ts {
                        fields.insert(s.clone(), Box::new(t.clone().to_type(type_context.clone(), &self.type_decs)?));
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
                _ => return Err(InvalidExprError::InvalidTopLevelExpr(expr.head_string()).into()),
            }
        }

        println!("{:?}", self.type_decs);
        Ok(())
    }
}
