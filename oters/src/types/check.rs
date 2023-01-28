use std::collections::{HashMap, VecDeque};

use super::{insert_dec, traverse_path, Type, TypeContext, TypeError};
use crate::export::{ExportEnums, ExportFns, ExportStructs};
use crate::exprs::{
    BOpcode, Expr, InvalidExprError, InvalidPatternError, LetBinding, UOpcode, VarContext,
};
use crate::parser::ast::{PExpr, Pattern, Program};
use daggy::Dag;

use anyhow::Result;
use daggy::petgraph::visit::{EdgeRef, IntoEdges};

#[derive(Debug)]
pub struct ProgramChecker {
    // These represent the global top-level declarations
    // Each node represents a pair containing type declarations, and value declarations
    type_decs: Dag<HashMap<String, Type>, String>,
    value_decs: Dag<HashMap<String, Type>, String>,

    fresh_type_var: i32,
    substitutions: Vec<(String, Type)>,
    current_path: Vec<String>,

    pub checked_exprs: Dag<Vec<LetBinding>, String>,
}

impl ProgramChecker {
    pub fn new() -> Self {
        let mut type_decs = Dag::new();
        let td_root = type_decs.add_node(HashMap::new());
        assert_eq!(td_root, 0.into());

        let mut value_decs = Dag::new();
        let vd_root = value_decs.add_node(HashMap::new());
        assert_eq!(vd_root, 0.into());

        let mut checked_exprs = Dag::new();
        let ce_root = checked_exprs.add_node(Vec::new());
        assert_eq!(ce_root, 0.into());

        ProgramChecker {
            type_decs,
            value_decs,
            fresh_type_var: 0,
            substitutions: Vec::new(),
            current_path: Vec::new(),
            checked_exprs,
        }
    }

    fn insert_checked_expr(&mut self, binding: LetBinding, path: &Vec<String>) {
        let mut node = 0.into();
        for module in path {
            let mut child = None;
            for edge in self.checked_exprs.edges(node) {
                if edge.weight() == module {
                    child = Some(edge.target());
                    break;
                }
            }
            node = if let Some(child) = child {
                child
            } else {
                self.checked_exprs
                    .add_child(node, module.clone(), Vec::new())
                    .1
            };
        }

        self.checked_exprs[node].push(binding);
    }

    pub fn type_check_program(
        &mut self,
        program: &Program,
        path: Vec<String>,
        exports: Option<(ExportFns, ExportStructs, ExportEnums)>,
    ) -> Result<()> {
        self.current_path = path.clone();

        // Add exports to declarations
        match exports {
            Some(exports) => {
                for (s, (_, args, ret)) in exports.0 {
                    let t = if args.len() == 1 {
                        Type::Function(Box::new(args[0].clone()), Box::new(ret))
                    } else {
                        Type::Function(
                            Box::new(Type::Tuple(args.into_iter().map(|t| Box::new(t)).collect())),
                            Box::new(ret),
                        )
                    };
                    insert_dec(&mut self.value_decs, s, t, &path);
                }

                let mut type_decs = HashMap::new();
                for (s, map) in exports.1 {
                    type_decs.insert(s, Type::Struct(map));
                }

                let mut variant_map = HashMap::new();
                for (enum_name, map) in exports.2 {
                    // Add variants to map
                    for (variant, _) in &map {
                        variant_map.insert(variant.clone(), enum_name.clone());
                    }
                    insert_dec(&mut self.type_decs, enum_name, Type::Enum(map), &path);
                }
            }
            None => (),
        }

        // Checks expressions in order of appearance
        // Expression cannot reference value declared below it
        //   => Cyclic declarations are disallowed
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
                        &path,
                    )?;

                    // Check that the type is well formed
                    t.well_formed(TypeContext::new())?;

                    // Add it to the type declarations
                    insert_dec(&mut self.type_decs, t_id.clone(), t, &path);
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
                    insert_dec(&mut self.type_decs, id.clone(), t, &path);
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
                    insert_dec(&mut self.type_decs, id.clone(), t, &path);
                }
                PExpr::Let(pat, expr) => {
                    let mut e = Expr::from_pexpr(*expr.clone())?;

                    let mut ctx = VarContext::new();

                    // If e is recursive within one time step
                    let ret_type = match pat {
                        Pattern::Var(var, _) => {
                            if matches!(e, Expr::Fn(..)) && e.is_static_recursive(&var) {
                                // Add a recursive variable into the context
                                let t = Type::Function(
                                    Box::new(Type::GenericVar(self.fresh_type_var(), false)),
                                    Box::new(Type::GenericVar(self.fresh_type_var(), false)),
                                );
                                ctx.push_var(var.clone(), t.clone());
                                Some(t)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };

                    if matches!(ret_type, None) {
                        // If e is a recursive variable then convert it into a fix expression
                        for var in pat.vars() {
                            let fix_var = format!("rec_{}", var);
                            // Recursive variables are translated to fix points to ensure guarded recursion...
                            let (is_rec, rec_e) = e.clone().substitute(
                                &var,
                                &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(
                                    Vec::new(),
                                    fix_var.clone(),
                                ))))),
                            );
                            e = if is_rec {
                                Expr::Fix(fix_var, Box::new(rec_e))
                            } else {
                                e
                            };
                        }
                    }

                    // Convert to Rattus λ✓
                    e = e.single_tick(0);

                    // Type check the expression
                    let t = self.infer(&e, ctx)?;
                    let (t_pat, vars) = self.check_pattern(pat.clone())?;

                    // Unify binding pattern type and expression type
                    let mut constraints = VecDeque::from([(t.clone(), t_pat)]);

                    // Unify recrusive variable and return type
                    if let Some(rec_t) = ret_type {
                        constraints.push_back((t, rec_t));
                    }
                    let mut subs = unify(constraints)?;

                    self.substitutions.append(&mut subs);

                    // Unify the substitutions
                    self.unify_subs()?;

                    for (var, t) in vars {
                        // Apply the substitutions
                        let mut t = t.apply_subs(&self.substitutions);

                        // Convert free variables to generics
                        let free_vars = t.get_free_vars();

                        t = if !free_vars.is_empty() {
                            Type::Generic(free_vars.into_iter().collect(), Box::new(t))
                        } else {
                            t
                        };

                        // Make sure the resulting type is well formed
                        t.well_formed(TypeContext::new())?;
                        println!("{:?}::{}: {}", path, var, t);

                        // Add it to the map of value declarations
                        insert_dec(&mut self.value_decs, var, t, &path)
                    }

                    self.substitutions = Vec::new();

                    self.insert_checked_expr(LetBinding::Let(pat.clone(), e), &path);
                }
                PExpr::LetAndWith(pat1, e1, pat2, e2, e3) => {
                    let (x, y) = match (pat1, pat2) {
                        (Pattern::Var(x, _), Pattern::Var(y, _)) => (x, y),
                        _ => {
                            return Err(TypeError::InvalidMutuallyRecursiveDefinition(
                                Expr::from_pexpr(*e1.clone())?,
                            )
                            .into())
                        }
                    };

                    // Γ ⊢ 𝑒₃ ∶ 𝑡₂
                    let mut e3 = Expr::from_pexpr(*e3.clone())?;
                    e3 = e3.single_tick(0);

                    let mut t2 = self.infer(&e3, VarContext::new())?;
                    self.unify_subs()?;
                    t2 = t2.apply_subs(&self.substitutions);

                    // Γ, y∶ Str<𝑡₂> ⊢ 𝑒₁ ∶ Str<𝑡₁>
                    let mut e1 = Expr::from_pexpr(*e1.clone())?;

                    let fix_var = format!("rec_{}", x);
                    let (is_rec, rec_e) = e1.clone().substitute(
                        &x,
                        &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(
                            Vec::new(),
                            fix_var.clone(),
                        ))))),
                    );
                    e1 = if is_rec {
                        Expr::Fix(fix_var, Box::new(rec_e))
                    } else {
                        return Err(TypeError::InvalidMutuallyRecursiveDefinition(e1).into());
                    };
                    e1 = e1.single_tick(0);
                    insert_dec(
                        &mut self.value_decs,
                        y.clone(),
                        Type::Fix(
                            format!("_f{}", y),
                            Box::new(Type::Tuple(vec![
                                Box::new(t2.clone()),
                                Box::new(Type::FixVar(format!("_f{}", y))),
                            ])),
                        ),
                        &path,
                    );

                    let mut t1 = self.infer(&e1, VarContext::new())?;
                    self.unify_subs()?;
                    t1 = t1.apply_subs(&self.substitutions);
                    t1.well_formed(TypeContext::new())?;
                    match t1.clone() {
                        Type::Fix(_, tup) => match *tup {
                            Type::Tuple(ts) => {
                                if !matches!(*ts[1], Type::FixVar(_)) {
                                    return Err(
                                        TypeError::InvalidMutuallyRecursiveDefinition(e1).into()
                                    );
                                }
                            }
                            _ => {
                                return Err(TypeError::InvalidMutuallyRecursiveDefinition(e1).into())
                            }
                        },
                        _ => return Err(TypeError::InvalidMutuallyRecursiveDefinition(e1).into()),
                    }
                    println!("{:?}::{}: {}", path, x, t1);
                    insert_dec(&mut self.value_decs, x.clone(), t1, &path);

                    // Γ, x∶ Str<𝑡₁> ⊢ 𝑒₂ ∶ Str<𝑡₂>
                    let mut e2 = Expr::from_pexpr(*e2.clone())?;
                    let fix_var = format!("rec_{}", y);
                    let (is_rec, rec_e) = e2.clone().substitute(
                        &y,
                        &Expr::Adv(Box::new(Expr::Unbox(Box::new(Expr::Var(
                            Vec::new(),
                            fix_var.clone(),
                        ))))),
                    );
                    e2 = if is_rec {
                        Expr::Fix(fix_var, Box::new(rec_e))
                    } else {
                        return Err(TypeError::InvalidMutuallyRecursiveDefinition(e2).into());
                    };

                    e2 = e2.single_tick(0);
                    let mut s_t2 = self.infer(&e2, VarContext::new())?;
                    self.unify_subs()?;
                    s_t2 = s_t2.apply_subs(&self.substitutions);

                    let mut subs = unify(VecDeque::from([(
                        s_t2.clone(),
                        Type::Fix(
                            format!("_f{}", y),
                            Box::new(Type::Tuple(vec![
                                Box::new(t2.clone()),
                                Box::new(Type::FixVar(format!("_f{}", y))),
                            ])),
                        ),
                    )]))?;
                    self.substitutions.append(&mut subs);
                    s_t2.well_formed(TypeContext::new())?;

                    println!("{:?}::{}: {}", path, y, s_t2);
                    insert_dec(&mut self.value_decs, y.clone(), s_t2, &path);

                    self.substitutions = Vec::new();

                    self.insert_checked_expr(
                        LetBinding::LetAndWith(x.clone(), e1, y.clone(), e2, e3),
                        &path,
                    );
                }
                PExpr::Use(use_path, is_type) => {
                    let last_elem = use_path.iter().last().unwrap();
                    let use_path: &Vec<String> = &use_path[0..use_path.len() - 1].into();
                    if last_elem == "*" {
                        // Copy all elements in path to root node
                        let types = traverse_path(&mut self.type_decs, &use_path)?;
                        let values = traverse_path(&mut self.value_decs, &use_path)?;
                        if !values.is_empty() {
                            self.insert_checked_expr(
                                LetBinding::Use(use_path.clone(), last_elem.clone()),
                                &path,
                            )
                        }
                        for (s, t) in types {
                            insert_dec(&mut self.type_decs, s, t, &path);
                        }
                        for (s, t) in values {
                            insert_dec(&mut self.value_decs, s, t, &path);
                        }
                    } else {
                        // Copy last_elem to root node
                        let dag = if *is_type {
                            &mut self.type_decs
                        } else {
                            self.insert_checked_expr(
                                LetBinding::Use(use_path.clone(), last_elem.clone()),
                                &path,
                            );
                            &mut self.value_decs
                        };
                        let node = traverse_path(dag, &use_path)?;
                        insert_dec(
                            dag,
                            last_elem.clone(),
                            node.get(last_elem).unwrap().clone(),
                            &path,
                        );
                    }
                }
                _ => return Err(InvalidExprError::InvalidTopLevelExpr(expr.head_string()).into()),
            }
        }

        Ok(())
    }

    // Type Inference for a let expression according to Rattus λ rules
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
                    // Unify head of list with list type
                    BOpcode::Cons => unify(VecDeque::from([
                        (t2.clone(), Type::List(Box::new(t1.clone()))),
                        (t2.clone(), t3.clone()),
                    ]))?,
                    // For comparison operators the return type is always bool
                    BOpcode::Eq | BOpcode::Lt | BOpcode::Gt => {
                        unify(VecDeque::from([(t1.clone(), t2.clone())]))?
                    }
                    // Unify operands and result types
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
                match t {
                    Type::Int => Ok(Type::Int),
                    Type::Float => Ok(Type::Float),
                    Type::GenericVar(..) => Ok(t),
                    _ => Err(TypeError::ImproperType(Type::Int, t).into()),
                }
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
                ctx = ctx.one_tick()?;
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
                let t_ret = Type::GenericVar(self.fresh_type_var(), false);

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
                let t_ret = Type::GenericVar(self.fresh_type_var(), false);

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
                let a = Type::GenericVar(self.fresh_type_var(), false);

                let fix_var = self.fresh_type_var();

                let mut subs = unify(VecDeque::from([(
                    t.clone(),
                    Type::Fix(fix_var.clone(), Box::new(a.clone())),
                )]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                match t {
                    Type::Fix(_, t_) => Ok(t_.sub_delay_fix(&fix_var)),
                    Type::GenericVar(..) => Ok(a),
                    _ => Err(TypeError::ImproperType(
                        Type::Fix("α".to_string(), Box::new(Type::Unit)),
                        t,
                    )
                    .into()),
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
                let t_ret = Type::GenericVar(self.fresh_type_var(), false);

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
            Struct(path, id, v) => {
                let mut fields = Vec::new();
                for (f, e) in v {
                    fields.push((f, self.infer(e, ctx.clone())?));
                }
                let path = if path.is_empty() {
                    &self.current_path
                } else {
                    path
                };

                let t_struct = match traverse_path(&self.type_decs, path)?.get(id) {
                    Some(t) => match t {
                        // Instantiate generic variables
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var(), false);
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
                        if map.len() != fields.len() {
                            return Err(TypeError::StructFieldsDoNotMatch(id.clone()).into());
                        }
                        // Unify struct expression's fields with those of the declared struct
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
            Variant(path, id, o) => {
                let t = match o {
                    Some(e) => Some(self.infer(&e, ctx.clone())?),
                    None => None,
                };

                let enum_name = &path[path.len() - 1];
                let mut path: &Vec<std::string::String> = &path[0..path.len() - 1].into();
                path = if path.is_empty() {
                    &self.current_path
                } else {
                    path
                };

                let t_enum = match traverse_path(&self.type_decs, &path)?.get(enum_name) {
                    Some(enm) => match enm {
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var(), false);
                                t_ = t_.sub_generic(&arg, &fresh_t);
                            }

                            t_
                        }
                        t => t.clone(),
                    },
                    None => return Err(TypeError::UserTypeNotFound(enum_name.clone()).into()),
                };

                // Unify variant type with the declared enum's type
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
            Fn(pat, e) => {
                let (t_pat, vars) = self.check_pattern(pat.clone())?;
                let mut ctx = ctx.clone();
                ctx = ctx.one_tick()?;
                for (var, t_var) in vars {
                    ctx.push_var(var.clone(), t_var.clone());
                }

                let t_fn_ret = self.infer(e, ctx.clone())?;

                ctx.apply_subs(&self.substitutions);

                // After unification the variable's type may have changed
                t_pat.apply_subs(&self.substitutions);

                Ok(Type::Function(Box::new(t_pat), Box::new(t_fn_ret)))
            }
            Fix(alpha, e) => {
                let mut ctx = ctx.clone().stable()?;
                let t_ret = Type::GenericVar(self.fresh_type_var(), false);
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
            Seq(e1, e2) => {
                let t = self.infer(&e1, ctx.clone())?;

                let mut subs = unify(VecDeque::from([(t.clone(), Type::Unit)]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(self.infer(e2, ctx)?)
            }
            App(e1, e2) => {
                let t1 = self.infer(e1, ctx.clone())?;
                let t2 = self.infer(e2, ctx.clone())?;
                let t3 = Type::GenericVar(self.fresh_type_var(), false);

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
                let field_type = Type::GenericVar(self.fresh_type_var(), false);
                let map = HashMap::from([(f.clone(), Box::new(field_type.clone()))]);

                // Struct unification only requires a subset of fields to match
                let mut subs = unify(VecDeque::from([(t.clone(), Type::Struct(map))]))?;
                self.substitutions.append(&mut subs);
                ctx.apply_subs(&self.substitutions);

                Ok(field_type)
            }
            Match(e, v) => {
                let t_e = self.infer(e, ctx.clone())?;
                let t_ret = Type::GenericVar(self.fresh_type_var(), false);

                let mut constraints = VecDeque::new();
                for (p, e_p) in v {
                    let (t_p, p_vars) = self.check_pattern(p.clone())?;
                    constraints.push_back((t_e.clone(), t_p.clone()));

                    // Add p's variables into the context
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
            Var(path, var) => match ctx.clone().get_var(var) {
                Ok((t, true)) => {
                    // Instantiate generic variables of ∀ types
                    let (t_, constraints) = t.instantiate();
                    let mut subs = unify(constraints)?;
                    self.substitutions.append(&mut subs);
                    ctx.apply_subs(&subs);

                    Ok(t_)
                }
                // Variable access is in wrong time step but we can assume stable
                Ok((t, false)) => {
                    let (t, mut constraints) = t.instantiate();
                    let (t_, mut constraints_) = t.stablify(&Vec::new());
                    if !t_.is_stable()? {
                        return Err(TypeError::InvalidVariableAccess(var.clone()).into());
                    }
                    constraints.append(&mut constraints_);

                    let mut subs = unify(constraints)?;
                    self.substitutions.append(&mut subs);
                    ctx.apply_subs(&subs);

                    Ok(t_)
                }
                // Check that it's not a global variable
                Err(e) => {
                    let path = if path.is_empty() {
                        &self.current_path
                    } else {
                        path
                    };
                    match traverse_path(&mut self.value_decs, path)?.get(var) {
                        Some(t) => match t {
                            // Instantiate generic variables
                            Type::Generic(scheme, t) => {
                                let mut t_ = *t.clone();

                                for arg in scheme {
                                    let fresh_t = Type::GenericVar(self.fresh_type_var(), false);
                                    t_ = t_.sub_generic(arg, &fresh_t);
                                }

                                Ok(t_)
                            }
                            t => Ok(t.clone()),
                        },
                        None => Err(e),
                    }
                }
            },
            LetIn(pat, e1, e2) => {
                let mut ctx1 = ctx.clone();
                // If e is recursive within one time step
                let t_e_ = match pat {
                    Pattern::Var(var, _) => {
                        if matches!(*e1.clone(), Expr::Fn(..)) && e1.is_static_recursive(&var) {
                            // Add a recursive variable into the context
                            let t = Type::Function(
                                Box::new(Type::GenericVar(self.fresh_type_var(), false)),
                                Box::new(Type::GenericVar(self.fresh_type_var(), false)),
                            );
                            ctx1.push_var(var.clone(), t.clone());
                            Some(t)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                // Similar to top level let
                let t_e = self.infer(&e1, ctx1.clone())?;
                let (t_pat, vars) = self.check_pattern(pat.clone())?;

                let mut constraints = VecDeque::from([(t_e.clone(), t_pat)]);
                if let Some(rec_t) = t_e_ {
                    constraints.push_back((t_e.clone(), rec_t));
                }
                let mut subs = unify(constraints)?;
                self.substitutions.append(&mut subs);

                self.unify_subs()?;

                let mut ctx = ctx.clone();
                for (let_var, t_var) in vars {
                    let mut t_var = t_var.apply_subs(&self.substitutions);

                    let mut t_var_free_vars = t_var.get_free_vars();
                    let ctx_free_vars = ctx1.get_free_vars();

                    for var in ctx_free_vars {
                        t_var_free_vars.remove(&var);
                    }

                    t_var = if t_var_free_vars.is_empty() {
                        t_var
                    } else {
                        Type::Generic(
                            t_var_free_vars.into_iter().collect(),
                            Box::new(t_var.clone()),
                        )
                    };

                    ctx.push_var(let_var.clone(), t_var);
                }

                Ok(self.infer(e2, ctx)?)
            }
            Location(_) => Err(InvalidExprError::IllegalLocation.into()),
        }
    }

    fn infer_binop(&mut self, t1: Type, op: BOpcode, t2: Type) -> Result<Type> {
        use BOpcode::*;
        use Type::*;
        use TypeError::ImproperType;
        match (t1, op, t2) {
            (GenericVar(v1, _), Add, GenericVar(..)) => Ok(GenericVar(v1, true)),
            (Int, Add, Int) | (Int, Add, GenericVar(..)) | (GenericVar(..), Add, Int) => Ok(Int),
            (Float, Add, Float) | (Float, Add, GenericVar(..)) | (GenericVar(..), Add, Float) => {
                Ok(Float)
            }
            (t1, Add, t2) => Err(ImproperType(t1, t2).into()),

            (GenericVar(v1, _), Sub, GenericVar(..)) => Ok(GenericVar(v1, true)),
            (Int, Sub, Int) | (Int, Sub, GenericVar(..)) | (GenericVar(..), Sub, Int) => Ok(Int),
            (Float, Sub, Float) | (Float, Sub, GenericVar(..)) | (GenericVar(..), Sub, Float) => {
                Ok(Float)
            }
            (t1, Sub, t2) => Err(ImproperType(t1, t2).into()),

            (GenericVar(v1, _), Mul, GenericVar(..)) => Ok(GenericVar(v1, true)),
            (Int, Mul, Int) | (Int, Mul, GenericVar(..)) | (GenericVar(..), Mul, Int) => Ok(Int),
            (Float, Mul, Float) | (Float, Mul, GenericVar(..)) | (GenericVar(..), Mul, Float) => {
                Ok(Float)
            }
            (t1, Mul, t2) => Err(ImproperType(t1, t2).into()),

            (GenericVar(v1, _), Div, GenericVar(..)) => Ok(GenericVar(v1, true)),
            (Int, Div, Int) | (Int, Div, GenericVar(..)) | (GenericVar(..), Div, Int) => Ok(Int),
            (Float, Div, Float) | (Float, Div, GenericVar(..)) | (GenericVar(..), Div, Float) => {
                Ok(Float)
            }
            (t1, Div, t2) => Err(ImproperType(t1, t2).into()),

            (GenericVar(..), Mod, GenericVar(..))
            | (Int, Mod, Int)
            | (Int, Mod, GenericVar(..))
            | (GenericVar(..), Mod, Int) => Ok(Int),
            (t1, Mod, t2) => Err(ImproperType(t1, t2).into()),

            (GenericVar(var, s), Cons, GenericVar(..)) => Ok(List(Box::new(GenericVar(var, s)))),
            (t1, Cons, List(t2)) => {
                if t1 == *t2 {
                    Ok(List(Box::new(t1)))
                } else {
                    Err(ImproperType(List(Box::new(t1)), List(t2)).into())
                }
            }
            (t, Cons, GenericVar(..)) => Ok(List(Box::new(t))),
            (t1, Cons, t2) => Err(ImproperType(List(Box::new(t1)), t2).into()),

            (t1, Eq, t2) => {
                match t1 {
                    Int | Float | Bool | String | GenericVar(..) => (),
                    _ => return Err(ImproperType(GenericVar("α".to_string(), false), t2).into()),
                }
                match t2 {
                    Int | Float | Bool | String | GenericVar(..) => (),
                    _ => return Err(ImproperType(GenericVar("α".to_string(), false), t2).into()),
                }

                if t1 == t2 || matches!(t1, GenericVar(..)) || matches!(t2, GenericVar(..)) {
                    Ok(Bool)
                } else {
                    Err(ImproperType(t1, t2).into())
                }
            }

            (GenericVar(..), Lt, GenericVar(..))
            | (Int, Lt, Int)
            | (Int, Lt, GenericVar(..))
            | (GenericVar(..), Lt, Int)
            | (Float, Lt, Float)
            | (Float, Lt, GenericVar(..))
            | (GenericVar(..), Lt, Float) => Ok(Bool),
            (t1, Lt, t2) => Err(ImproperType(t1, t2).into()),

            (GenericVar(..), Gt, GenericVar(..))
            | (Int, Gt, Int)
            | (Int, Gt, GenericVar(..))
            | (GenericVar(..), Gt, Int)
            | (Float, Gt, Float)
            | (Float, Gt, GenericVar(..))
            | (GenericVar(..), Gt, Float) => Ok(Bool),
            (t1, Gt, t2) => Err(ImproperType(t1, t2).into()),

            (GenericVar(..), And, GenericVar(..))
            | (Bool, And, Bool)
            | (Bool, And, GenericVar(..))
            | (GenericVar(..), And, Bool) => Ok(Bool),
            (Bool, And, t2) => Err(ImproperType(Bool, t2).into()),
            (t1, And, Bool) => Err(ImproperType(Bool, t1).into()),
            (t1, And, _) => Err(ImproperType(Bool, t1).into()),

            (GenericVar(..), Or, GenericVar(..))
            | (Bool, Or, Bool)
            | (Bool, Or, GenericVar(..))
            | (GenericVar(..), Or, Bool) => Ok(Bool),
            (Bool, Or, t2) => Err(ImproperType(Bool, t2).into()),
            (t1, Or, Bool) => Err(ImproperType(Bool, t1).into()),
            (t1, Or, _) => Err(ImproperType(Bool, t1).into()),
        }
    }

    // Returns the pattern's type and a mapping of variables declared in the pattern to their type
    pub fn check_pattern(&mut self, p: Pattern) -> Result<(Type, HashMap<String, Type>)> {
        use Pattern::*;
        match p {
            Underscore => Ok((
                Type::GenericVar(self.fresh_type_var(), false),
                HashMap::new(),
            )),
            Bool(_) => Ok((Type::Bool, HashMap::new())),
            Int(_) => Ok((Type::Int, HashMap::new())),
            Float(_) => Ok((Type::Float, HashMap::new())),
            String(_) => Ok((Type::String, HashMap::new())),
            Unit => Ok((Type::Unit, HashMap::new())),
            Tuple(v) => {
                let mut vars = HashMap::new();
                let mut types = Vec::new();
                // Check each sub_pattern
                for p in v {
                    let (t, p_vars) = self.check_pattern(*p.clone())?;
                    types.push(Box::new(t));
                    for (var, t_var) in p_vars {
                        // Check that the same variable name does not appear twice in the same pattern
                        match vars.insert(var.clone(), t_var) {
                            None => (),
                            Some(_) => {
                                return Err(InvalidPatternError::SimultaneousPatternBinding(
                                    var.clone(),
                                    *p.clone(),
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
                                    *p.clone(),
                                )
                                .into())
                            }
                        }
                    }
                }

                // If empty list then return generic list
                match t_list {
                    None => Ok((
                        Type::List(Box::new(Type::GenericVar(self.fresh_type_var(), false))),
                        vars,
                    )),
                    Some(t) => Ok((Type::List(Box::new(t)), vars)),
                }
            }
            Variant(path, c, o) => {
                let (t, vars) = match o {
                    Some(p) => {
                        let (t_, p_vars) = self.check_pattern(*p.clone())?;
                        (Some(t_), p_vars)
                    }
                    None => (None, HashMap::new()),
                };

                let enum_name = &path[path.len() - 1];
                let mut path: &Vec<std::string::String> = &path[0..path.len() - 1].into();
                path = if path.is_empty() {
                    &self.current_path
                } else {
                    path
                };
                let t_enum = match traverse_path(&mut self.type_decs, path)?.get(enum_name) {
                    Some(enm) => match enm {
                        // Instantiate generic variables
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var(), false);
                                t_ = t_.sub_generic(&arg, &fresh_t);
                            }

                            t_
                        }
                        t => t.clone(),
                    },
                    None => return Err(TypeError::UserTypeNotFound(enum_name.clone()).into()),
                };

                match &t_enum {
                    // Unify enum variant type and pattern type
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
            Struct(path, id, v) => {
                let mut fields = Vec::new();
                let mut vars = HashMap::new();
                // Get each field's pattern's type and variable substitutions
                for (f, p) in v.clone() {
                    if f == ".." {
                        break;
                    }
                    let (t, p_vars) = match &p {
                        None => {
                            let t = Type::GenericVar(self.fresh_type_var(), false);
                            (t.clone(), HashMap::from([(f.clone(), t)]))
                        }
                        Some(p) => self.check_pattern(*p.clone())?,
                    };

                    fields.push((f, t));
                    for (var, t_var) in p_vars {
                        match vars.insert(var.clone(), t_var) {
                            None => (),
                            Some(_) => {
                                return Err(InvalidPatternError::SimultaneousPatternBinding(
                                    var.clone(),
                                    Struct(path, id, v),
                                )
                                .into())
                            }
                        }
                    }
                }

                let path = if path.is_empty() {
                    &self.current_path
                } else {
                    &path
                };
                let t_struct = match traverse_path(&self.type_decs, path)?.get(&id) {
                    Some(t) => match t {
                        // Instantiate generic variables
                        Type::Generic(scheme, t) => {
                            let mut t_ = *t.clone();
                            for arg in scheme {
                                let fresh_t = Type::GenericVar(self.fresh_type_var(), false);
                                t_ = t_.sub_generic(&arg, &fresh_t);
                            }

                            t_
                        }
                        t => t.clone(),
                    },

                    None => return Err(TypeError::UserTypeNotFound(id.clone()).into()),
                };

                // Unify pattern types and struct's definition field types
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
                                Cons(p1.clone(), p2.clone()),
                            )
                            .into())
                        }
                    }
                }

                let t_ret = match (t1.clone(), t2.clone()) {
                    (Type::GenericVar(var, stability), Type::GenericVar(..)) => {
                        Type::List(Box::new(Type::GenericVar(var, stability)))
                    }
                    (t1, Type::List(t2)) => {
                        if t1 == *t2 {
                            Type::List(Box::new(t1))
                        } else {
                            return Err(TypeError::ImproperType(
                                Type::List(Box::new(t1)),
                                Type::List(t2),
                            )
                            .into());
                        }
                    }
                    (t, Type::GenericVar(..)) => Type::List(Box::new(t)),
                    (t1, t2) => {
                        return Err(TypeError::ImproperType(Type::List(Box::new(t1)), t2).into())
                    }
                };

                let mut subs = unify(VecDeque::from([
                    (t2.clone(), Type::List(Box::new(t1.clone()))),
                    (t2.clone(), t_ret.clone()),
                ]))?;
                self.substitutions.append(&mut subs);

                Ok((t_ret, vars1))
            }
            Stream(x, xs_p) => {
                let (t, mut vars) = self.check_pattern(*x.clone())?;
                match &*xs_p {
                    Var(xs, _) => {
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
                                    Stream(x, xs_p),
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
                                Cons(p1.clone(), p2.clone()),
                            )
                            .into())
                        }
                    }
                }

                let mut subs = unify(VecDeque::from([(t1.clone(), t2.clone())]))?;
                self.substitutions.append(&mut subs);

                Ok((t2, vars1))
            }
            Var(var, stability) => {
                let t = Type::GenericVar(self.fresh_type_var(), stability);
                Ok((t.clone(), HashMap::from([(var, t)])))
            }
        }
    }

    // Returns a fresh type variable for generic variable names
    fn fresh_type_var(&mut self) -> String {
        let x = self.fresh_type_var;
        self.fresh_type_var += 1;

        format!("__t{}", x)
    }

    // Unify the substitutions
    fn unify_subs(&mut self) -> Result<()> {
        // Map from generic type variable to indeces in the substitutions vec
        let mut sub_map = HashMap::new();
        let mut i = 0;
        while i < self.substitutions.len() {
            let (x1, t) = self.substitutions[i].clone();

            match sub_map.get_mut(&x1) {
                None => {
                    sub_map.insert(x1.clone(), vec![i]);
                }
                Some(indices) => {
                    // Same variable binds to different types
                    let mut constraints = VecDeque::new();
                    // Unify all the types bound the same variable
                    for index in indices.clone() {
                        constraints.push_back((t.clone(), self.substitutions[index].1.clone()));
                    }
                    let subs = unify(constraints)?;
                    for sub in subs {
                        if !self.substitutions.contains(&sub) {
                            self.substitutions.push(sub);
                        }
                    }

                    (*indices).push(i);
                }
            }
            i += 1;
        }
        Ok(())
    }
}

// Unification given a list of constraints
fn unify(mut constraints: VecDeque<(Type, Type)>) -> Result<Vec<(String, Type)>> {
    use Type::*;
    match &constraints.pop_front() {
        None => Ok(Vec::new()),
        Some((t1, t2)) => {
            if t1 == t2 {
                unify(constraints)
            } else {
                match (t1, t2) {
                    // Replace non-stable variable with the stable one
                    (GenericVar(_, true), GenericVar(beta, false)) => {
                        sub_constraints(&mut constraints, &t2, &t1);
                        let mut subs = unify(constraints)?;

                        subs.push((beta.clone(), t1.clone()));
                        Ok(subs)
                    }
                    // Replace non-stable variable with the stable one
                    (GenericVar(alpha, false), GenericVar(_, true)) => {
                        sub_constraints(&mut constraints, &t1, &t2);
                        let mut subs = unify(constraints)?;

                        subs.push((alpha.clone(), t2.clone()));
                        Ok(subs)
                    }

                    (GenericVar(alpha, true), t2) => {
                        // Collapse all unstable generic vars to stable ones
                        let (t2_, constraints_) = t2.stablify(&Vec::new());

                        // If the resulting type is still not stable, then don't unify
                        if !t2_.is_stable().unwrap() {
                            return Err(TypeError::ExpectedStableType(t2.clone()).into());
                        }

                        for (var, val) in constraints_ {
                            sub_constraints(&mut constraints, &var, &val);
                        }
                        sub_constraints(&mut constraints, &GenericVar(alpha.clone(), true), &t2);
                        let mut subs = unify(constraints)?;

                        subs.push((alpha.clone(), t2.clone()));
                        Ok(subs)
                    }

                    (t1, GenericVar(alpha, true)) => {
                        let (t1_, constraints_) = t1.stablify(&Vec::new());
                        if !t1_.is_stable().unwrap() {
                            return Err(TypeError::ExpectedStableType(t1.clone()).into());
                        }

                        for (var, val) in constraints_ {
                            sub_constraints(&mut constraints, &var, &val);
                        }
                        sub_constraints(&mut constraints, &t2, &t1);
                        let mut subs = unify(constraints)?;

                        subs.push((alpha.clone(), t1.clone()));
                        Ok(subs)
                    }

                    (GenericVar(alpha, false), t2) => {
                        sub_constraints(&mut constraints, &t1, &t2);
                        let mut subs = unify(constraints)?;

                        subs.push((alpha.clone(), t2.clone()));
                        Ok(subs)
                    }
                    (t1, GenericVar(alpha, false)) => {
                        sub_constraints(&mut constraints, &t2, &t1);
                        let mut subs = unify(constraints)?;

                        subs.push((alpha.clone(), t1.clone()));
                        Ok(subs)
                    }
                    (Tuple(v1), Tuple(v2)) => {
                        if v1.len() != v2.len() {
                            Err(TypeError::ImproperType(t1.clone(), t2.clone()).into())
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
                        // Instantiate the bound variables and unify the resulting types
                        let mut t1 = *t1.clone();
                        for arg in scheme1 {
                            // Probably wrong
                            let fresh_t = Type::GenericVar(arg.to_string(), false);
                            t1 = t1.sub_generic(&arg, &fresh_t);
                        }

                        let mut t2 = *t2.clone();
                        for arg in scheme2 {
                            // Probably wrong
                            let fresh_t = Type::GenericVar(arg.to_string(), false);
                            t2 = t2.sub_generic(&arg, &fresh_t);
                        }

                        constraints.push_front((t1, t2));

                        unify(constraints)
                    }
                    (Struct(map1), Struct(map2)) => {
                        // A struct needs to unify with a subset of fields of the other one
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
                    _ => Err(TypeError::ImproperType(t1.clone(), t2.clone()).into()),
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
