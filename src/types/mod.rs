mod check;
mod tests;

use crate::parser::ast::Expr;

use std::collections::HashMap;

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
    pub fn is_stable(&self) -> bool {
        use self::Type::*;
        match self {
            Unit | Int | Float | String | Bool => true, // All primitive types are Stable
            Tuple(v) => v.iter().fold(true, |mut acc, t| {
                acc = acc && t.is_stable();
                acc
            }),
            List(t) => t.is_stable(),
            Function(..) => false, // Functions can have temporal values in their closure
            Delay(..) => false,    // Delayed values are inherently temporal
            Stable(..) => true,    // Stable values wrap any type, making it atemporal
            Fix(..) => false,      // The fix point type argument is implictly a delay type
            FixVar(..) => false,
            Struct(m) => m.iter().fold(true, |mut acc, (_, t)| {
                acc = acc && t.is_stable();
                acc
            }),
            Enum(m) => m.iter().fold(true, |mut acc, (_, o)| {
                let t_stable = match o {
                    Some(t) => t.is_stable(),
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

                params_stable && t.is_stable()
            }
            GenericVar(b, _) => *b,
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

    pub fn stable(&self) -> Self {
        let terms = self
            .terms
            .iter()
            .fold(Vec::new(), |mut context, term| match term {
                VarTerm::Tick => context,
                VarTerm::Var(x, t) => {
                    if t.is_stable() {
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

    pub fn extend(&mut self, params: &mut GenericParams) {
        self.types.append(params);
    }

    pub fn well_formed(&self, t: &Type) -> bool {
        use Type::*;
        match t {
            Unit | Int | Float | String | Bool => true, // All primitive types are well formed
            Tuple(v) => v.iter().fold(true, |mut acc, t| {
                acc = acc && self.well_formed(t);
                acc
            }),
            List(t) => self.well_formed(t),
            Function(t1, t2) => self.well_formed(t1) && self.well_formed(t2),
            Delay(t) => self.well_formed(t),
            Stable(t) => self.well_formed(t),
            Fix(_, t) => self.well_formed(t),
            FixVar(..) => true, // Fix point vars go into the term context as they have unchanging types
            Struct(m) => m.iter().fold(true, |mut acc, (_, t)| {
                acc = acc && self.well_formed(t);
                acc
            }),
            Enum(m) => m.iter().fold(true, |mut acc, (_, o)| {
                let t_stable = match o {
                    Some(t) => self.well_formed(t),
                    None => true,
                };
                acc = acc && t_stable;
                acc
            }),
            Generic(params, t) => {
                // the ∀ type in System F
                let mut context = self.clone();
                context.extend(&mut params.clone());

                context.well_formed(t)
            }
            GenericVar(b, s) => {
                // makes sure that the generic parameter has been declared
                for (b_prime, s_prime) in &self.types {
                    if b == b_prime && s == s_prime {
                        return true;
                    }
                }
                false
            }
        }
    }
}
