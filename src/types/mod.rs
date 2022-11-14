mod errors;
mod tests;

use crate::parser::ast::TypeExpr;
use errors::TypeError;

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
    FixVar(String),
    Generic(GenericParams, Box<Type>), // A pair of generic parameters with the type definition
    GenericVar(bool, String),
    Struct(HashMap<String, Box<Type>>), // A map from the struct fields to their respective type
    Enum(HashMap<String, Option<Box<Type>>>), // A map from each variant constructor to an Option
    App(Box<Type>, Vec<Box<Type>>), // A type applied to a generic (e.g. Stream<int> => App(Stream, int))
}

#[derive(Debug, PartialEq)]
pub enum VarTerm {
    Tick,
    Var(String, Type),
}

#[derive(Debug, PartialEq)]
pub struct VarContext {
    terms: Vec<VarTerm>,
    ticks: Vec<usize>, // Locations of ticks within the context
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
            FixVar(..) => self.clone(),
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
        }
    }

    pub fn is_stable(&self, t_decs: &HashMap<String, Type>) -> bool {
        use self::Type::*;
        match self {
            Unit | Int | Float | String | Bool => true, // All primitive types are Stable
            Tuple(v) => v.iter().fold(true, |mut acc, t| {
                acc = acc && t.is_stable(t_decs);
                acc
            }),
            List(t) => t.is_stable(t_decs),
            Function(..) => false, // Functions can have temporal values in their closure
            Delay(..) => false,    // Delayed values are inherently temporal
            Stable(..) => true,    // Stable values wrap any type, making it atemporal
            Fix(..) => false,      // The fix point type argument is implictly a delay type
            FixVar(..) => false,
            Struct(m) => m.iter().fold(true, |mut acc, (_, t)| {
                acc = acc && t.is_stable(t_decs);
                acc
            }),
            Enum(m) => m.iter().fold(true, |mut acc, (_, o)| {
                let t_stable = match o {
                    Some(t) => t.is_stable(t_decs),
                    None => true,
                };
                acc = acc && t_stable;
                acc
            }),
            Generic(params, t) => {
                let params_stable = params.iter().fold(true, |mut acc, (b, _)| {
                    acc = acc && *b;
                    acc
                });

                params_stable && t.is_stable(t_decs)
            }
            GenericVar(b, _) => *b,
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
                        Generic(params[args.len()..].to_vec(), Box::new(result)).is_stable(t_decs)
                    }
                }
                _ => unreachable!("Cannot do type application on non-Generic types"),
            },
        }
    }
}

impl TypeExpr {
    fn to_type(self, t_context: TypeContext, t_decs: &HashMap<String, Type>) -> Result<Type> {
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
                    Some(t) => {
                        if v.is_empty() {
                            Ok(t.clone())
                        } else {
                            Ok(App(Box::new(t.clone()), args))
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

impl VarContext {
    pub fn new() -> Self {
        Self {
            terms: Vec::new(),
            ticks: Vec::new(),
        }
    }

    pub fn stable(&self, t_decs: &HashMap<String, Type>) -> Self {
        let terms = self
            .terms
            .iter()
            .fold(Vec::new(), |mut context, term| match term {
                VarTerm::Tick => context,
                VarTerm::Var(x, t) => {
                    if t.is_stable(t_decs) {
                        context.push(VarTerm::Var(x.clone(), t.clone()));
                        context
                    } else {
                        context
                    }
                }
            });

        Self {
            terms,
            ticks: Vec::new(),
        }
    }
}

impl TypeContext {
    pub fn new() -> Self {
        TypeContext { types: Vec::new() }
    }

    fn extend(&mut self, params: &mut GenericParams) {
        self.types.append(params);
    }

    pub fn well_formed(&self, t: &Type, t_decs: &HashMap<String, Type>) -> bool {
        use Type::*;
        match t {
            Unit | Int | Float | String | Bool => true, // All primitive types are well formed
            Tuple(v) => v.iter().fold(true, |mut acc, t| {
                acc = acc && self.well_formed(t, t_decs);
                acc
            }),
            List(t) => self.well_formed(t, t_decs),
            Function(t1, t2) => self.well_formed(t1, t_decs) && self.well_formed(t2, t_decs),
            Delay(t) => self.well_formed(t, t_decs),
            Stable(t) => self.well_formed(t, t_decs),
            Fix(_, t) => self.well_formed(t, t_decs),
            FixVar(..) => true, // Fix point vars go into the term context as they have unchanging types
            Struct(m) => m.iter().fold(true, |mut acc, (_, t)| {
                acc = acc && self.well_formed(t, t_decs);
                acc
            }),
            Enum(m) => m.iter().fold(true, |mut acc, (_, o)| {
                let t_stable = match o {
                    Some(t) => self.well_formed(t, t_decs),
                    None => true,
                };
                acc = acc && t_stable;
                acc
            }),
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
                        return true;
                    }
                }
                false // May want to give specific errors instead of bool response
            }
            App(t, v) => match &**t {
                // Concrete type applied to a generic type e.g. Option<int>
                Generic(params, t) => {
                    if v.len() > params.len() {
                        // List of args cannot be longer than allowed params
                        return false;
                    }
                    for (i, t) in v.iter().enumerate() {
                        // Check that a non-stable type is not passed as a stable arg
                        if params[i].0 && !t.is_stable(t_decs) {
                            return false;
                        }
                        if !self.well_formed(t, t_decs) {
                            return false;
                        }
                    }
                    self.well_formed(&Generic(params.clone(), t.clone()), t_decs)
                }
                _ => false,
            },
        }
    }
}
