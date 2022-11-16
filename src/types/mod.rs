pub mod check;
mod errors;
mod tests;

use crate::parser::ast::TypeExpr;
pub use errors::TypeError;

use std::collections::HashMap;

use anyhow::Result;

type GenericParams = Vec<(bool, String)>; // Pairs of the parameter name and a stability bool

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
    FixVar(String), // These have their own types
    Generic(GenericParams, Box<Type>), // A pair of generic parameters with the type definition
    GenericVar(bool, String),
    Struct(HashMap<String, Box<Type>>), // A map from the struct fields to their respective type
    Enum(HashMap<String, Option<Box<Type>>>), // A map from each variant constructor to an Option
    App(Box<Type>, Vec<Box<Type>>), // A type applied to a generic (e.g. Stream<int> => App(Stream, int))
    User(String),                   // The name of an Enum, Struct or Type alias
}

#[derive(Clone)]
pub struct TypeContext {
    // Θ type contexts that hold generic type variables from System F
    types: GenericParams,
}

impl Type {
    #![allow(unreachable_code)]
    fn substitute(&self, v: &String, t: &Type) -> Type {
        use Type::*;
        match self {
            Unit | Int | Float | String | Bool => self.clone(),
            Tuple(ts) => Tuple(ts.iter().map(|t_| Box::new(t_.substitute(v, t))).collect()),
            List(t_) => List(Box::new(t_.substitute(v, t))),
            Function(t1, t2) => {
                Function(Box::new(t1.substitute(v, t)), Box::new(t2.substitute(v, t)))
            }
            Delay(t_) => Delay(Box::new(t_.substitute(v, t))),
            Stable(t_) => Stable(Box::new(t_.substitute(v, t))),
            Fix(alpha, t_) => Fix(alpha.clone(), Box::new(t_.substitute(v, t))),
            FixVar(_) => self.clone(),
            Generic(..) => !unreachable!("Can't have nested Generic Types"),
            GenericVar(.., id) => {
                if id == v {
                    // Substitute if the GenericVar matches
                    t.clone()
                } else {
                    self.clone()
                }
            }
            Struct(map) => Struct(
                map.iter()
                    .map(|(id, t_)| (id.clone(), Box::new(t_.substitute(v, t))))
                    .collect(),
            ),
            Enum(map) => Enum(
                map.iter()
                    .map(|(c, o)| {
                        (
                            c.clone(),
                            o.as_ref().map(|t_| Box::new(t_.substitute(v, t))),
                        )
                    })
                    .collect(),
            ),
            App(t1, args) => App(
                Box::new(t1.substitute(v, t)),
                args.iter()
                    .map(|t_| Box::new(t_.substitute(v, t)))
                    .collect(),
            ),
            User(_) => self.clone(),
        }
    }

    pub fn is_stable(&self, t_decs: &HashMap<String, Type>) -> Result<bool> {
        use self::Type::*;
        match self {
            Unit | Int | Float | String | Bool => Ok(true), // All primitive types are Stable
            Tuple(v) => {
                let mut result = true;
                for t in v {
                    result = result && t.is_stable(t_decs)?;
                }
                Ok(result)
            }
            List(t) => t.is_stable(t_decs),
            Function(..) => Ok(false), // Functions can have temporal values in their closure
            Delay(..) => Ok(false),    // Delayed values are inherently temporal
            Stable(..) => Ok(true),    // Stable values wrap any type, making it atemporal
            Fix(..) => Ok(false),      // The fix point type argument is implictly a delay type
            FixVar(..) => Ok(true),
            Struct(m) => {
                let mut result = true;
                for (_, t) in m.iter() {
                    result = result && t.is_stable(t_decs)?;
                }
                Ok(result)
            }

            Enum(m) => {
                let mut result = true;
                for (_, o) in m.iter() {
                    let t_stable = match o {
                        Some(t) => t.is_stable(t_decs)?,
                        None => true,
                    };

                    result = result && t_stable;
                }
                Ok(result)
            }
            Generic(params, t) => {
                let params_stable = params.iter().fold(true, |mut acc, (b, _)| {
                    acc = acc && *b;
                    acc
                });

                Ok(params_stable && t.is_stable(t_decs)?)
            }
            GenericVar(b, _) => Ok(*b),
            App(t, args) => match &**t {
                Generic(params, t_) => {
                    // To find if a type application is stable we have to perform the substitution
                    let mut result = Unit;
                    for (i, a) in args.iter().enumerate() {
                        result = t_.substitute(&params[i].1, &a);
                    }

                    if params.len() == args.len() {
                        result.is_stable(t_decs)
                    } else {
                        Ok(Generic(params[args.len()..].to_vec(), Box::new(result))
                            .is_stable(t_decs)?)
                    }
                }
                User(id) => match t_decs.get(id) {
                    Some(t) => App(Box::new(t.clone()), args.clone()).is_stable(t_decs),
                    None => Err(TypeError::UserTypeNotFound(id.clone()).into()),
                },
                _ => Err(TypeError::ImproperTypeArguments.into()),
            },
            User(id) => match t_decs.get(id) {
                Some(t) => t.clone().is_stable(t_decs),
                None => Err(TypeError::UserTypeNotFound(id.clone()).into()),
            },
        }
    }
}

impl TypeExpr {
    pub fn to_type(self, t_context: TypeContext, t_decs: &HashMap<String, Type>) -> Result<Type> {
        use Type::*;
        use TypeExpr::*;
        match self {
            TEUnit => Ok(Unit),
            TEInt => Ok(Int),
            TEFloat => Ok(Float),
            TEString => Ok(String),
            TEBool => Ok(Bool),
            TETuple(v) => {
                let mut types = Vec::new();
                for t_expr in v {
                    types.push(Box::new(
                        t_expr.to_owned().to_type(t_context.clone(), t_decs)?,
                    ));
                }
                Ok(Tuple(types))
            }
            TEList(t) => Ok(List(Box::new(t.to_type(t_context, t_decs)?))),
            TEUser(id, v) => {
                // All generic arguments and user declared types are "dereferenced"
                for (b, s) in &t_context.types {
                    // Check if the ident is a Generic Argument
                    if id == s.clone() {
                        return Ok(GenericVar(*b, s.clone()));
                    }
                }

                let mut args = Vec::new(); // The generic type arguments to a user defined type
                for t_expr in &v {
                    args.push(Box::new(
                        t_expr.to_owned().to_type(t_context.clone(), t_decs)?,
                    ));
                }

                match t_decs.get(&id) {
                    // Find the user type in the type declarations
                    Some(_) => {
                        if v.is_empty() {
                            Ok(User(id))
                        } else {
                            Ok(App(Box::new(User(id)), args))
                        }
                    }
                    None => Err(TypeError::UserTypeNotFound(id).into()),
                }
            }
            TEFunction(t1, t2) => Ok(Function(
                Box::new(t1.to_type(t_context.clone(), t_decs)?),
                Box::new(t2.to_type(t_context.clone(), t_decs)?),
            )),
            TEDelay(t) => Ok(Delay(Box::new(t.to_type(t_context, t_decs)?))),
            TEStable(t) => Ok(Stable(Box::new(t.to_type(t_context, t_decs)?))),
            TEFix(alpha, t) => Ok(Fix(alpha, Box::new(t.to_type(t_context, t_decs)?))),
            TEVar(alpha) => Ok(FixVar(alpha)),
        }
    }
}

impl crate::exprs::VarContext {
    pub fn stable(&self, t_decs: &HashMap<String, Type>) -> Result<Self> {
        use crate::exprs::VarTerm;

        let mut terms = Vec::new();
        for term in &self.terms {
            match term {
                VarTerm::Tick => (),
                VarTerm::Var(x, t) => {
                    if t.is_stable(t_decs)? {
                        terms.push(VarTerm::Var(x.clone(), t.clone()));
                    }
                }
            }
        }

        Ok(Self {
            terms,
            ticks: Vec::new(),
        })
    }
}

impl TypeContext {
    pub fn new() -> Self {
        TypeContext { types: Vec::new() }
    }

    fn extend(&mut self, params: &mut GenericParams) {
        self.types.append(params);
    }

    pub fn well_formed(&self, t: &Type, t_decs: &HashMap<String, Type>) -> Result<()> {
        use Type::*;
        match t {
            Unit | Int | Float | String | Bool => Ok(()), // All primitive types are well formed
            Tuple(v) => {
                for t in v {
                    self.well_formed(t, t_decs)?;
                }
                Ok(())
            }
            List(t) => self.well_formed(t, t_decs),
            Function(t1, t2) => {
                self.well_formed(t1, t_decs)?;
                self.well_formed(t2, t_decs)
            }
            Delay(t) => self.well_formed(t, t_decs),
            Stable(t) => self.well_formed(t, t_decs),
            Fix(alpha, t) => {
                let mut context = self.clone();
                context.extend(&mut vec![(true, alpha.clone())]);

                context.well_formed(t, t_decs)
            }

            FixVar(alpha) => {
                // makes sure that the generic parameter has been declared
                for (_, s) in &self.types {
                    if  alpha == s {
                        return Ok(());
                    }
                }
                Err(TypeError::FixedPointVariableNotFound(alpha.clone()).into())
            }
            Struct(m) => {
                for (_, t) in m {
                    self.well_formed(t, t_decs)?;
                }
                Ok(())
            }
            Enum(m) => {
                for (_, o) in m {
                    match o {
                        None => (),
                        Some(t) => self.well_formed(t, t_decs)?,
                    };
                }
                Ok(())
            }
            Generic(params, t) => {
                // the ∀ type in System F
                let mut context = self.clone();
                context.extend(&mut params.clone());

                context.well_formed(t, t_decs)
            }
            GenericVar(b, s) => {
                // makes sure that the generic parameter has been declared
                for (b_prime, s_prime) in &self.types {
                    if b == b_prime && s == s_prime {
                        return Ok(());
                    }
                }
                Err(TypeError::GenericVariableNotFound(s.clone()).into())
            }
            App(t, v) => match &**t {
                // Concrete type applied to a generic type e.g. Option<int>
                Generic(params, t) => {
                    if v.len() > params.len() {
                        // List of args cannot be longer than allowed params
                        return Err(TypeError::ImproperTypeArguments.into());
                    }
                    for (i, t) in v.iter().enumerate() {
                        // Check that a non-stable type is not passed as a stable arg
                        if params[i].0 && !t.is_stable(t_decs).unwrap() {
                            return Err(TypeError::ImproperUnstableType(format!("{:?}", t)).into());
                        }
                        self.well_formed(t, t_decs)?;
                    }
                    self.well_formed(&Generic(params.clone(), t.clone()), t_decs)
                }
                User(id) => match t_decs.get(id) {
                    Some(t) => self.well_formed(&App(Box::new(t.clone()), v.clone()), t_decs),
                    None => Err(TypeError::UserTypeNotFound(id.clone()).into()),
                },

                _ => Err(TypeError::ImproperTypeArguments.into()),
            },
            User(id) => match t_decs.get(id) {
                Some(t) => self.well_formed(t, t_decs),
                None => Err(TypeError::UserTypeNotFound(id.clone()).into()),
            },
        }
    }
}
