pub mod check;
mod errors;
mod tests;
mod utils;

use crate::parser::span::SpTypeExpr;
use crate::{errors::SpError, parser::ast::TypeExpr};
pub use errors::TypeError;
pub use utils::*;

use std::collections::{HashMap, HashSet, VecDeque};

use daggy::{Dag, NodeIndex};

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
    FixVar(String),                           // These have their own types
    Generic(Vec<String>, Box<Type>), // A pair of generic parameters with the type definition
    GenericVar(String, bool),        // bool indicates where the variable is stable or not
    Struct(HashMap<String, Box<Type>>), // A map from the struct fields to their respective type
    Enum(HashMap<String, Option<Box<Type>>>), // A map from each variant constructor to an Option
}

#[derive(Clone, Debug)]
pub struct TypeContext {
    // Θ type contexts that hold generic type variables as in System F
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
    // Converts TExpr to Type
    pub fn from_texpr(
        t: SpTypeExpr,
        t_context: TypeContext, // Holds generic type variables
        t_decs: &Dag<HashMap<String, Type>, String>,
    ) -> Result<Type, SpError> {
        use Type::*;
        use TypeExpr::*;
        let (t, span) = (*t.term, t.span);
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
                        t_expr,
                        t_context.clone(),
                        t_decs,
                    )?));
                }
                Ok(Tuple(types))
            }
            TEList(t) => Ok(List(Box::new(Type::from_texpr(t, t_context, t_decs)?))),
            // All generic arguments and user declared types are "dereferenced"
            TEUser(path, id, v) => {
                for s in &t_context.vars {
                    // Check if the ident is a Generic Argument
                    if id == s.clone() {
                        return Ok(GenericVar(s.clone(), false));
                    }
                }

                // If it's a delcared type copy its definition
                let map = traverse_path(&t_decs, &path).map_err(|e| SpError::new(e, span))?;
                let mut t = match map.get(&id) {
                    Some(t) => t.clone(),
                    None => return Err(SpError::new(TypeError::UserTypeNotFound(id).into(), span)),
                };

                // Add generic type parameters
                if v.len() > 0 {
                    match t.clone() {
                        Generic(args, t_) => {
                            for (i, t_expr) in v.iter().enumerate() {
                                t = t_.sub_generic(
                                    &args[i],
                                    &Type::from_texpr(t_expr.clone(), t_context.clone(), t_decs)?,
                                )
                            }
                            if v.len() < args.len() {
                                t = Generic(args[v.len()..].to_vec(), Box::new(t))
                            }
                        }
                        _ => {
                            return Err(SpError::new(TypeError::ImproperTypeArguments.into(), span))
                        }
                    }
                }

                Ok(t)
            }
            TEFunction(t1, t2) => Ok(Function(
                Box::new(Type::from_texpr(t1, t_context.clone(), t_decs)?),
                Box::new(Type::from_texpr(t2, t_context.clone(), t_decs)?),
            )),
            TEDelay(t) => Ok(Delay(Box::new(Type::from_texpr(t, t_context, t_decs)?))),
            TEStable(t) => Ok(Stable(Box::new(Type::from_texpr(t, t_context, t_decs)?))),
        }
    }

    // Convert the type definitions from type expressions
    pub fn from_typedef(
        id: String,
        params: Vec<String>,
        t: SpTypeExpr,
        t_decs: &Dag<HashMap<String, Type>, String>,
        path: &Vec<String>,
    ) -> Result<Type, SpError> {
        // Add type being defined in the declarations for recursive types
        let mut t_decs = t_decs.clone();
        let current_map = traverse_path(&t_decs, path).unwrap_or(HashMap::new());
        let root_id: NodeIndex = 0.into();
        t_decs[root_id] = current_map;
        insert_dec(
            &mut t_decs,
            id.clone(),
            Type::GenericVar(id.clone(), false),
            &Vec::new(),
        );

        // Add type parameters to the type context
        let t_context = TypeContext {
            vars: params.clone(),
        };
        // Convert TExpr to Type
        let mut t = Type::from_texpr(t, t_context, &t_decs)?;

        // If type is recursive turn it into a Fix type
        let fix_var = format!("rec_{}", id);
        let (b, t_) = t.sub_delay(&id, &fix_var);
        if b {
            t = Type::Fix(fix_var, Box::new(t_));
        }

        // If generic then turn it into a Generic Type
        if params.len() > 0 {
            t = Type::Generic(params, Box::new(t))
        }

        Ok(t)
    }

    // Convert the type definitions from struct expressions
    pub fn from_structdef(
        id: String,
        params: Vec<String>,
        fields: Vec<(String, SpTypeExpr)>,
        t_decs: &Dag<HashMap<String, Type>, String>,
    ) -> Result<Type, SpError> {
        let mut t_decs = t_decs.clone();
        insert_dec(
            &mut t_decs,
            id.clone(),
            Type::GenericVar(id.clone(), false),
            &Vec::new(),
        );

        let t_context = TypeContext {
            vars: params.clone(),
        };

        let mut field_map = HashMap::new();
        for (s, t) in fields {
            field_map.insert(
                s,
                Box::new(Type::from_texpr(t, t_context.clone(), &t_decs)?),
            );
        }

        let mut t = Type::Struct(field_map);
        if params.len() > 0 {
            t = Type::Generic(params, Box::new(t))
        }

        Ok(t)
    }

    // Convert the type definitions from enum expressions
    pub fn from_enumdef(
        id: String,
        params: Vec<String>,
        variants: Vec<(String, Option<SpTypeExpr>)>,
        t_decs: &Dag<HashMap<String, Type>, String>,
    ) -> Result<Type, SpError> {
        let mut t_decs = t_decs.clone();
        insert_dec(
            &mut t_decs,
            id.clone(),
            Type::GenericVar(id.clone(), false),
            &Vec::new(),
        );

        let t_context = TypeContext {
            vars: params.clone(),
        };

        let mut var_map = HashMap::new();
        for (s, o) in variants {
            let t = match o {
                None => None,
                Some(t_expr) => Some(Box::new(Type::from_texpr(
                    t_expr,
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

    // Substitutes FixVar(fix_var) for Delay(var) and if the substitution took place
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
                GenericVar(id, stability) => {
                    if id == var && !stability {
                        (true, FixVar(fix_var.clone()))
                    } else {
                        let (b, t_) = t.sub_delay(var, fix_var);
                        (b, Delay(Box::new(t_)))
                    }
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
            Generic(args, t) => {
                if args.contains(var) {
                    // var is now a different bound variable
                    (false, self.clone())
                } else {
                    let (b, t_) = t.sub_delay(var, fix_var);
                    (b, Generic(args.clone(), Box::new(t_)))
                }
            }
            GenericVar(..) => (false, self.clone()),
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
                        None => {
                            variants.insert(v.clone(), None);
                        }
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

    // Substitutes Delay(Fix(fix_var)) for FixVar(fix_var)
    pub(super) fn sub_delay_fix(&self, fix_var: &String) -> Type {
        use Type::*;
        match self {
            Unit | Int | Float | String | Bool => self.clone(),
            Tuple(ts) => Tuple(
                ts.iter()
                    .map(|t_| Box::new(t_.sub_delay_fix(fix_var)))
                    .collect(),
            ),
            List(t_) => List(Box::new(t_.sub_delay_fix(fix_var))),
            Function(t1, t2) => Function(
                Box::new(t1.sub_delay_fix(fix_var)),
                Box::new(t2.sub_delay_fix(fix_var)),
            ),
            Delay(t) => match &**t {
                Fix(alpha, _) => {
                    if alpha == fix_var {
                        FixVar(alpha.clone())
                    } else {
                        Delay(Box::new(t.sub_delay_fix(fix_var)))
                    }
                }
                _ => Delay(Box::new(t.sub_delay_fix(fix_var))),
            },
            Stable(t_) => Stable(Box::new(t_.sub_delay_fix(fix_var))),
            Fix(alpha, t_) => {
                if alpha == fix_var {
                    self.clone()
                } else {
                    Fix(alpha.clone(), Box::new(t_.sub_delay_fix(&alpha)))
                }
            }
            FixVar(_) => self.clone(),
            Generic(args, t_) => {
                if args.contains(fix_var) {
                    // var is now a different bound variable
                    self.clone()
                } else {
                    Generic(args.clone(), Box::new(t_.sub_delay_fix(fix_var)))
                }
            }
            GenericVar(..) => self.clone(),
            Struct(map) => Struct(
                map.iter()
                    .map(|(id, t_)| (id.clone(), Box::new(t_.sub_delay_fix(fix_var))))
                    .collect(),
            ),
            Enum(map) => Enum(
                map.iter()
                    .map(|(c, o)| {
                        (
                            c.clone(),
                            o.as_ref().map(|t_| Box::new(t_.sub_delay_fix(fix_var))),
                        )
                    })
                    .collect(),
            ),
        }
    }

    // Substitutes GenericVar(Var) for t
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
            Generic(args, t_) => {
                if args.contains(var) {
                    // var is now a different bound variable
                    self.clone()
                } else {
                    Generic(args.clone(), Box::new(t_.sub_generic(var, t)))
                }
            }
            GenericVar(id, stability) => {
                // Don't substitute unstable type for stable var
                if *stability && !t.is_stable().unwrap() {
                    return self.clone();
                }
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

    // Is the type well formed, i.e. are all generic variables bound
    pub fn well_formed(&self, t_context: TypeContext) -> anyhow::Result<()> {
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
            GenericVar(var, _) => {
                if t_context.vars.contains(var) {
                    Ok(())
                } else {
                    Err(TypeError::GenericVariableNotFound(var.clone()).into())
                }
            }
        }
    }

    pub fn is_stable(&self) -> anyhow::Result<bool> {
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
            GenericVar(_, stability) => Ok(*stability),
        }
    }

    // Returns a set of the free type variables in the type
    pub(super) fn get_free_vars(&self) -> HashSet<String> {
        use Type::*;
        match self {
            Unit | Int | Float | String | Bool => HashSet::new(), // All primitive types are Stable
            Tuple(v) => {
                let mut result = HashSet::new();
                for t in v {
                    result.extend(t.get_free_vars());
                }
                result
            }
            List(t) => t.get_free_vars(),
            Function(t1, t2) => {
                let mut result = t1.get_free_vars();
                result.extend(t2.get_free_vars());
                result
            } // Functions can have temporal values in their closure
            Delay(t) => t.get_free_vars(),
            Stable(t) => t.get_free_vars(),
            Fix(_, t) => t.get_free_vars(),
            FixVar(..) => HashSet::new(),
            Struct(m) => {
                let mut result = HashSet::new();
                for (_, t) in m.iter() {
                    result.extend(t.get_free_vars());
                }
                result
            }
            Enum(m) => {
                let mut result = HashSet::new();
                for (_, o) in m.iter() {
                    match o {
                        Some(t) => result.extend(t.get_free_vars()),
                        None => (),
                    };
                }
                result
            }
            Generic(vars, t) => {
                let mut result = t.get_free_vars();
                for var in vars {
                    result.remove(var);
                }
                result
            }
            GenericVar(var, _) => HashSet::from([var.clone()]),
        }
    }

    pub(super) fn instantiate(&self) -> (Type, VecDeque<(Type, Type)>) {
        use Type::*;
        let mut fresh_var = 0;
        match self {
            Generic(scheme, t) => {
                let mut t_ = *t.clone();

                let mut constraints = VecDeque::new();
                for arg in scheme {
                    let fresh_t = Type::GenericVar(format!("__ti{}", fresh_var), false);
                    t_ = t_.sub_generic(&arg, &fresh_t);
                    constraints.push_back((Type::GenericVar(arg.clone(), false), fresh_t));
                    fresh_var += 1;
                }
                (t_, constraints)
            }
            _ => (self.clone(), VecDeque::new()),
        }
    }
    pub(super) fn stablify(&self, bound_vars: &Vec<String>) -> (Type, VecDeque<(Type, Type)>) {
        use Type::*;
        match self {
            Unit | Int | Float | String | Bool | Delay(_) | Stable(_) | FixVar(_) | Fix(..) => {
                (self.clone(), VecDeque::new())
            }
            Tuple(tuple) => {
                let mut constraints = VecDeque::new();
                let mut types = Vec::new();
                for t in tuple {
                    let (t_, mut constraints_) = t.stablify(bound_vars);
                    types.push(Box::new(t_));
                    constraints.append(&mut constraints_);
                }
                (Tuple(types), constraints)
            }
            List(t) => {
                let (t_, constraints) = t.stablify(bound_vars);
                (List(Box::new(t_)), constraints)
            }
            Function(t1, t2) => {
                let (t1_, mut constraints) = t1.stablify(bound_vars);
                let (t2_, mut constraints_) = t2.stablify(bound_vars);
                constraints.append(&mut constraints_);
                (Function(Box::new(t1_), Box::new(t2_)), constraints)
            }
            Generic(gens, t) => {
                let (t_, constraints) = t.stablify(&gens);
                (Generic(gens.clone(), Box::new(t_)), constraints)
            }
            GenericVar(var, _) => {
                if bound_vars.contains(&var) {
                    (self.clone(), VecDeque::new())
                } else {
                    let t_ = GenericVar(format!("#__{}", var), true);
                    let constraints = VecDeque::from([(self.clone(), t_.clone())]);
                    (t_, constraints)
                }
            }
            Struct(map) => {
                let mut constraints = VecDeque::new();
                let mut fields = HashMap::new();
                for (field, t) in map {
                    let (t_, mut constraints_) = t.stablify(bound_vars);
                    fields.insert(field.clone(), Box::new(t_));
                    constraints.append(&mut constraints_);
                }
                (Struct(fields), constraints)
            }
            Enum(map) => {
                let mut constraints = VecDeque::new();
                let mut variants = HashMap::new();
                for (variant, opt) in map {
                    match opt {
                        None => {
                            variants.insert(variant.clone(), None);
                        }
                        Some(t) => {
                            let (t_, mut constraints_) = t.stablify(bound_vars);
                            variants.insert(variant.clone(), Some(Box::new(t_)));
                            constraints.append(&mut constraints_);
                        }
                    }
                }
                (Enum(variants), constraints)
            }
        }
    }

    // Given substiture GenericVars for Types according to subs
    pub(super) fn apply_subs(&self, subs: &Vec<(String, Type)>) -> Type {
        let mut result = self.clone();
        let mut prev = Type::Int;
        // A substitution may contain another substituition's variable
        // So perform the substitutions until no changes are registered
        while &result != &prev {
            prev = result.clone();
            for (var, t) in subs {
                result = result.sub_generic(&var, &t);
            }
            result = result.sub_delay_fix(&"".to_owned());
        }
        result
    }
}
