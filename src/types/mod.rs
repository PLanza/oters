pub mod check;
mod errors;
mod tests;

use crate::parser::ast::TypeExpr;
pub use errors::TypeError;

use std::collections::HashMap;

use anyhow::Result;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Float,
    String,
    Bool,
    Tuple(Vec<Box<Type>>),
    List(Box<Type>),
    Function(Box<Type>, Box<Type>),
    Delay(Box<Type>),
    Stable(Box<Type>),
    Fix(String, Box<Type>),
    FixVar(String),             // These have their own types
    Generic(Vec<String>, Box<Type>), // A pair of generic parameters with the type definition
    GenericVar(String),
    Struct(HashMap<String, Box<Type>>), // A map from the struct fields to their respective type
    Enum(HashMap<String, Option<Box<Type>>>), // A map from each variant constructor to an Option
}

#[derive(Clone, Debug)]
pub struct TypeContext {
    // Θ type contexts that hold generic type variables from System F
    vars: Vec<String>,
}

impl TypeContext {
    pub fn new() -> Self {
        TypeContext { vars: Vec::new() }
    }

    fn extend(&mut self, vars: &mut Vec<String>) {
        self.vars.append(vars);
    }
}

impl Type {
    pub fn from_texpr(
        t: TypeExpr,
        t_context: TypeContext,
        t_decs: &HashMap<String, Type>,
    ) -> Result<Type> {
        use Type::*;
        use TypeExpr::*;
        match t {
            TEUnit => Ok(Unit),
            TEInt => Ok(Int),
            TEFloat => Ok(Float),
            TEString => Ok(String),
            TEBool => Ok(Bool),
            TETuple(v) => {
                let mut types = Vec::new();
                for t_expr in v {
                    types.push(Box::new(Type::from_texpr(
                        *t_expr,
                        t_context.clone(),
                        t_decs,
                    )?));
                }
                Ok(Tuple(types))
            }
            TEList(t) => Ok(List(Box::new(Type::from_texpr(*t, t_context, t_decs)?))),
            // All generic arguments and user declared types are "dereferenced"
            TEUser(id, v) => {
                for s in &t_context.vars {
                    // Check if the ident is a Generic Argument
                    if id == s.clone() {
                        return Ok(GenericVar(s.clone()));
                    }
                }

                let mut t = match t_decs.get(&id) {
                    Some(t) => t.clone(),
                    None => return Err(TypeError::UserTypeNotFound(id).into()),
                };

                if v.len() > 0 {
                    match t.clone() {
                        Generic(args, t_) => {
                            for (i, t_expr) in v.iter().enumerate() {
                                t = t_.sub_generic(
                                    &args[i],
                                    &Type::from_texpr(*t_expr.clone(), t_context.clone(), t_decs)?,
                                )
                            }
                            if v.len() < args.len() {
                                t = Generic(args[v.len()..].to_vec(), Box::new(t))
                            }
                        }
                        _ => return Err(TypeError::ImproperTypeArguments.into()),
                    }
                }

                Ok(t)
            }
            TEFunction(t1, t2) => Ok(Function(
                Box::new(Type::from_texpr(*t1, t_context.clone(), t_decs)?),
                Box::new(Type::from_texpr(*t2, t_context.clone(), t_decs)?),
            )),
            TEDelay(t) => Ok(Delay(Box::new(Type::from_texpr(*t, t_context, t_decs)?))),
            TEStable(t) => Ok(Stable(Box::new(Type::from_texpr(*t, t_context, t_decs)?))),
        }
    }

    pub fn from_typedef(
        id: String,
        params: Vec<String>,
        t: TypeExpr,
        t_decs: &HashMap<String, Type>,
    ) -> Result<Type> {
        let mut t_decs = t_decs.clone();
        t_decs.insert(id.clone(), Type::GenericVar(id.clone()));

        let t_context = TypeContext {
            vars: params.clone(),
        };
        let mut t = Type::from_texpr(t, t_context, &t_decs)?;

        let fix_var = format!("rec_{}", id);
        let (b, t_) = t.sub_delay(&id, &fix_var);
        if b {
            t = Type::Fix(fix_var, Box::new(t_));
        }

        if params.len() > 0 {
            t = Type::Generic(params, Box::new(t))
        }

        Ok(t)
    }

    pub fn from_structdef(
        id: String,
        params: Vec<String>,
        fields: Vec<(String, Box<TypeExpr>)>,
        t_decs: &HashMap<String, Type>,
    ) -> Result<Type> {
        let mut t_decs = t_decs.clone();
        t_decs.insert(id.clone(), Type::GenericVar(id.clone()));

        let t_context = TypeContext {
            vars: params.clone(),
        };

        let mut field_map = HashMap::new();

        for (s, t) in fields {
            field_map.insert(
                s,
                Box::new(Type::from_texpr(*t, t_context.clone(), &t_decs)?),
            );
        }

        let mut t = Type::Struct(field_map);

        if params.len() > 0 {
            t = Type::Generic(params, Box::new(t))
        }

        Ok(t)
    }

    pub fn from_enumdef(
        id: String,
        params: Vec<String>,
        variants: Vec<(String, Option<Box<TypeExpr>>)>,
        t_decs: &HashMap<String, Type>,
    ) -> Result<Type> {
        let mut t_decs = t_decs.clone();
        t_decs.insert(id.clone(), Type::GenericVar(id.clone()));

        let t_context = TypeContext {
            vars: params.clone(),
        };

        let mut var_map = HashMap::new();

        for (s, o) in variants {
            let t = match o {
                None => None,
                Some(t_expr) => Some(Box::new(Type::from_texpr(
                    *t_expr,
                    t_context.clone(),
                    &t_decs,
                )?)),
            };
            var_map.insert(s, t);
        }

        let mut t = Type::Enum(var_map);

        if params.len() > 0 {
            t = Type::Generic(params, Box::new(t))
        }

        Ok(t)
    }

    // Substitutes a fix var for @(Var) and if the substitution took place
    fn sub_delay(&self, var: &String, fix_var: &String) -> (bool, Type) {
        use Type::*;
        match self {
            Unit | Int | Float | String | Bool => (false, self.clone()),
            Tuple(ts) => {
                let mut tuple = Vec::new();
                let mut b = false;
                for t in ts {
                    let (b_, t_) = t.sub_delay(var, fix_var);
                    b = b || b_;
                    tuple.push(Box::new(t_));
                }

                (b, Tuple(tuple))
            }
            List(t) => {
                let (b, t_) = t.sub_delay(var, fix_var);
                (b, List(Box::new(t_)))
            }
            Function(t1, t2) => {
                let (b1, t1_) = t1.sub_delay(var, fix_var);
                let (b2, t2_) = t2.sub_delay(var, fix_var);

                (b1 || b2, Function(Box::new(t1_), Box::new(t2_)))
            }

            Delay(t) => match &**t {
                GenericVar(id) => if id == var {
                    (true, FixVar(fix_var.clone()))
                } else {
                    let (b, t_) = t.sub_delay(var, fix_var);
                    (b, Delay(Box::new(t_)))
                }
                _ => {
                    let (b, t_) = t.sub_delay(var, fix_var);
                    (b, Delay(Box::new(t_)))
                }
            },
            Stable(t) => {
                let (b, t_) = t.sub_delay(var, fix_var);
                (b, Stable(Box::new(t_)))
            }
            Fix(alpha, t) => { 
                let (b, t_) = t.sub_delay(var, fix_var);
                (b, Fix(alpha.clone(), Box::new(t_)))
            }
            FixVar(_) => (false, self.clone()),
            Generic(args, t) => if args.contains(var) {
                // var is now a different bound variable
                (false, self.clone())
            } else  { 
                let (b, t_) = t.sub_delay(var, fix_var);
                (b, Generic(args.clone(), Box::new(t_)))
            }
            GenericVar(_) => (false, self.clone()),
            Struct(map) => {
                let mut fields = HashMap::new();
                let mut b = false;
                for (f, t) in map {
                    let (b_, t_) = t.sub_delay(var, fix_var);
                    b = b || b_;
                    fields.insert(f.clone(), Box::new(t_));
                }

                (b, Struct(fields))
            }
            Enum(map) => {
                let mut variants = HashMap::new();
                let mut b = false;
                for (v, o) in map {
                    match o {
                        None => { variants.insert(v.clone(), None); },
                        Some(t) => {
                            let (b_, t_) = t.sub_delay(var, fix_var);
                            b = b || b_;
                            variants.insert(v.clone(), Some(Box::new(t_)));
                        }
                    }
                }

                (b, Enum(variants))
            }
        }
    }

    pub fn sub_generic(&self, var: &String, t: &Type) -> Type {
        use Type::*;
        match self {
            Unit | Int | Float | String | Bool => self.clone(),
            Tuple(ts) => Tuple(
                ts.iter()
                    .map(|t_| Box::new(t_.sub_generic(var, t)))
                    .collect(),
            ),
            List(t_) => List(Box::new(t_.sub_generic(var, t))),
            Function(t1, t2) => Function(
                Box::new(t1.sub_generic(var, t)),
                Box::new(t2.sub_generic(var, t)),
            ),

            Delay(t_) => Delay(Box::new(t_.sub_generic(var, t))),
            Stable(t_) => Stable(Box::new(t_.sub_generic(var, t))),
            Fix(alpha, t_) => Fix(alpha.clone(), Box::new(t_.sub_generic(var, t))),
            FixVar(_) => self.clone(),
            Generic(args, t_) => if args.contains(var) {
                // var is now a different bound variable
                self.clone()
            } else {
                Generic(args.clone(), Box::new(t_.sub_generic(var, t)))
            }
            GenericVar(id) => {
                if id == var {
                    // Substitute if the GenericVar matches
                    t.clone()
                } else {
                    self.clone()
                }
            }
            Struct(map) => Struct(
                map.iter()
                    .map(|(id, t_)| (id.clone(), Box::new(t_.sub_generic(var, t))))
                    .collect(),
            ),
            Enum(map) => Enum(
                map.iter()
                    .map(|(c, o)| {
                        (
                            c.clone(),
                            o.as_ref().map(|t_| Box::new(t_.sub_generic(var, t))),
                        )
                    })
                    .collect(),
            ),
        }
    }

    pub fn well_formed(&self, t_context: TypeContext) -> Result<()> {
        use Type::*;
        match self {
            Unit | Int | Float | String | Bool => Ok(()), // All primitive types are well formed
            Tuple(v) => {
                for t in v {
                    t.well_formed(t_context.clone())?;
                }
                Ok(())
            }
            List(t) => t.well_formed(t_context),
            Function(t1, t2) => {
                t1.well_formed(t_context.clone())?;
                t2.well_formed(t_context)
            }
            Delay(t) => t.well_formed(t_context),
            Stable(t) => t.well_formed(t_context),
            Fix(alpha, t) => {
                let mut context = t_context.clone();
                context.extend(&mut vec![alpha.clone()]);

                t.well_formed(context)
            }
            // Fixed vars are lower case while generic vars are upper case
            FixVar(alpha) => {
                // makes sure that the generic parameter has been declared
                if t_context.vars.contains(alpha) {
                    Ok(())
                } else {
                    Err(TypeError::FixedPointVariableNotFound(alpha.clone()).into())
                }
            }
            Struct(m) => {
                for (_, t) in m {
                    t.well_formed(t_context.clone())?;
                }
                Ok(())
            }
            Enum(m) => {
                for (_, o) in m {
                    match o {
                        None => (),
                        Some(t) => t.well_formed(t_context.clone())?,
                    };
                }
                Ok(())
            }
            Generic(args, t) => {
                // the ∀ type in System F
                let mut context = t_context.clone();
                context.extend(&mut args.clone());

                t.well_formed(context)
            }
            // makes sure that the generic parameter has been declared
            GenericVar(s) => {
                if t_context.vars.contains(s) {
                    Ok(())
                } else {
                    Err(TypeError::GenericVariableNotFound(s.clone()).into())
                }
            }
        }
    }

    pub fn is_stable(&self) -> Result<bool> {
        use self::Type::*;
        match self {
            Unit | Int | Float | String | Bool => Ok(true), // All primitive types are Stable
            Tuple(v) => {
                let mut result = true;
                for t in v {
                    result = result && t.is_stable()?;
                }
                Ok(result)
            }
            List(t) => t.is_stable(),
            Function(..) => Ok(false), // Functions can have temporal values in their closure
            Delay(..) => Ok(false),    // Delayed values are inherently temporal
            Stable(..) => Ok(true),    // Stable values wrap any type, making it atemporal
            Fix(..) => Ok(false),      // The fix point type argument is implictly a delay type
            FixVar(..) => Ok(false),
            Struct(m) => {
                let mut result = true;
                for (_, t) in m.iter() {
                    result = result && t.is_stable()?;
                }
                Ok(result)
            }

            Enum(m) => {
                let mut result = true;
                for (_, o) in m.iter() {
                    let t_stable = match o {
                        Some(t) => t.is_stable()?,
                        None => true,
                    };

                    result = result && t_stable;
                }
                Ok(result)
            }
            Generic(..) => Ok(false),
            GenericVar(_) => Ok(false),
        }
    }
}
