use std::{collections::HashMap, sync::Mutex};

use lazy_static::lazy_static;
use proc_macro::{Span, TokenStream};
use quote::{quote, spanned::Spanned};
use syn::{parse_macro_input, FnArg, Ident, ItemFn, ReturnType};

lazy_static! {
    static ref EXPORTS: Mutex<HashMap<String, (Vec<ValueType>, ValueType)>> =
        Mutex::new(HashMap::new());
}

#[derive(Debug, Clone)]
enum ValueType {
    Bool,
    Int,
    Float,
    String,
    Unit,
    List(Box<ValueType>),
    Tuple(Vec<Box<ValueType>>),
    // Fn
    // Struct
    // Enum
}

#[proc_macro_attribute]
pub fn export_fn(_args: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    println!("{:?}", input);

    let fn_name = input.sig.ident;
    let args: Vec<FnArg> = input.sig.inputs.iter().cloned().collect();
    let return_type = input.sig.output;
    let fn_body = *input.block;

    let mut arg_names = Vec::new();
    let mut val_types = Vec::new();

    // Get the name and type of each argument
    for arg in args {
        match &arg {
            FnArg::Typed(syn::PatType { ty, pat, .. }) => {
                // Get the argument's name
                let arg_name = match *pat.clone() {
                    syn::Pat::Ident(ident) => ident.ident,
                    _ => {
                        return syn::Error::new(pat.__span(), "Argument must be an ident")
                            .to_compile_error()
                            .into()
                    }
                };
                arg_names.push(arg_name);

                // Get the type as a ValueType
                let val_ty = match to_val_type(*ty.clone()) {
                    Some(ty) => ty,
                    None => {
                        return syn::Error::new(ty.__span(), "Incompatible type")
                            .to_compile_error()
                            .into()
                    }
                };

                val_types.push(val_ty);
            }
            _ => {
                return syn::Error::new(arg.__span(), "Cannot export methods")
                    .to_compile_error()
                    .into()
            }
        }
    }

    // Get the return type as a ValueType
    let return_val = match return_type {
        ReturnType::Default => ValueType::Unit,
        ReturnType::Type(_, ty) => match to_val_type(*ty.clone()) {
            Some(ty) => ty,
            None => {
                return syn::Error::new(ty.__span(), "Incompatible type")
                    .to_compile_error()
                    .into()
            }
        },
    };

    let map_entry = (fn_name.to_string(), (val_types.clone(), return_val.clone()));

    // The indices for the function arguments
    let indices: Vec<usize> = (0..arg_names.len()).collect();

    let mut stmts = fn_body.stmts;
    // Convert the return statement to a Value
    let return_stmt = if stmts.len() == 0 {
        quote!(oters::export::Value::Unit)
    } else {
        match stmts[stmts.len() - 1].clone() {
            syn::Stmt::Expr(e) => {
                stmts.pop();
                to_ret_stmt(e, return_val)
            }
            _ => quote!(oters::export::Value::Unit),
        }
    };

    let match_arms = val_types.into_iter().map(|v| v.to_match_arm());

    // Form the exportable function
    let exportable = quote! {
        fn #fn_name(args: Vec<oters::export::Value>) -> oters::export::Value {
            #(let #arg_names = match args[#indices].clone() {
                #match_arms,
                _ => unreachable!()
            };)*
            #(#stmts)*
            #return_stmt
        }
    };

    EXPORTS.lock().unwrap().insert(map_entry.0, map_entry.1);

    println!("{}", exportable);

    TokenStream::from(exportable)
}

// Called after all #[export_fn]s and puts all the exported functions into a hashmap
#[proc_macro]
pub fn export_list(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut fn_names = Vec::new();
    let mut fn_pointers = Vec::new();
    let mut fn_types = Vec::new();
    let path = quote!(oters::types::Type);

    let map = EXPORTS.lock().unwrap().clone();

    for (name, (args, ret_val)) in map {
        fn_names.push(name.to_string());
        fn_pointers.push(Ident::new(&name, Span::call_site().into()));

        let ret_type = ret_val.to_type();
        if args.len() == 0 {
            fn_types.push(quote!(
            #path::Function(std::boxed::Box::new(#path::Unit), std::boxed::Box::new(#ret_type))
            ));
            break;
        }
        let mut iter = args.into_iter().rev();
        let arg_type = iter.next().unwrap().to_type();

        let mut fn_type = quote!(
        #path::Function(std::boxed::Box::new(#arg_type), std::boxed::Box::new(#ret_type))
        );
        for arg in iter {
            let arg_type = arg.to_type();
            fn_type = quote!(
            #path::Function(std::boxed::Box::new(#arg_type), std::boxed::Box::new(#fn_type))
            );
        }
        fn_types.push(fn_type);
    }

    quote! {
        lazy_static! {
            static ref EXPORT_FNS: HashMap<String, (fn(Vec<oters::export::Value>) -> oters::export::Value, #path)> =
                HashMap::from([#(
                        (#fn_names.to_string(), 
                         (#fn_pointers as fn(Vec<oters::export::Value>) -> oters::export::Value, 
                          #fn_types)
                         )
                        ),*]);
        }
    }
    .into()
}

fn to_val_type(ty: syn::Type) -> Option<ValueType> {
    Some(match ty {
        syn::Type::Path(ty) => {
            if ty.path.get_ident().is_none() {
                let ty = ty.path.segments.last().unwrap();
                if ty.ident != "Vec" {
                    return None;
                }
                let inner_ty = match &ty.arguments {
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        args,
                        ..
                    }) => match &args[0] {
                        syn::GenericArgument::Type(inner) => to_val_type(inner.clone())?,
                        _ => return None,
                    },
                    _ => return None,
                };
                return Some(ValueType::List(Box::new(inner_ty)));
            }
            match ty.path.get_ident().unwrap().to_string().as_str() {
                "bool" => ValueType::Bool,
                "i64" => ValueType::Int,
                "f64" => ValueType::Float,
                "String" => ValueType::String,
                _ => return None,
            }
        }
        syn::Type::Tuple(syn::TypeTuple { elems, .. }) => {
            let mut types = Vec::new();
            for ty in elems {
                types.push(Box::new(to_val_type(ty)?));
            }
            ValueType::Tuple(types)
        }
        _ => todo!(),
    })
}

fn to_ret_stmt(e: syn::Expr, return_val: ValueType) -> proc_macro2::TokenStream {
    let ret_ty = return_val.to_ident();
    match &return_val {
        ValueType::List(inner) => {
            let inner_ty = inner.to_ident();
            quote!(oters::export::Value::#ret_ty(
                #e
                .into_iter()
                .map(|v| std::boxed::Box::new(oters::export::Value::#inner_ty(v)))
                .collect::<std::collections::VecDeque<std::boxed::Box<oters::export::Value>>>()
            ))
        }
        ValueType::Tuple(inners) => {
            let inner_tys: Vec<Ident> = inners.into_iter().map(|t| t.to_ident()).collect();
            let indices: Vec<syn::Index> = (0..inners.len()).map(|i| syn::Index::from(i)).collect();
            quote!(oters::export::Value::#ret_ty(
                vec![#(std::boxed::Box::new(
                        oters::export::Value::#inner_tys(#e.#indices)
                )),*]
            ))
        }
        _ => quote!(oters::export::Value::#ret_ty(#e)),
    }
}

impl ValueType {
    fn to_ident(&self) -> Ident {
        use ValueType::*;
        match self {
            Unit => Ident::new("Unit", Span::call_site().into()),
            Bool => Ident::new("Bool", Span::call_site().into()),
            Int => Ident::new("Int", Span::call_site().into()),
            Float => Ident::new("Float", Span::call_site().into()),
            String => Ident::new("String", Span::call_site().into()),
            List(_) => Ident::new("List", Span::call_site().into()),
            Tuple(_) => Ident::new("Tuple", Span::call_site().into()),
        }
    }

    fn to_tokens(&self) -> proc_macro2::TokenStream {
        use ValueType::*;
        match self {
            Unit => quote!(()),
            Bool => quote!(bool),
            Int => quote!(i64),
            Float => quote!(f64),
            String => quote!(String),
            List(inner) => {
                let inner_ty = inner.to_tokens();
                quote!(Vec<#inner_ty>)
            }
            Tuple(inners) => {
                let inner_tys = inners.into_iter().map(|v| v.to_tokens());
                quote!((#(#inner_tys),*))
            }
        }
    }

    fn to_match_arm(&self) -> proc_macro2::TokenStream {
        let head = self.to_ident();

        use ValueType::*;
        match self {
            Unit | Bool | Int | Float | String => quote!(oters::export::Value::#head(v) => v),
            List(ty) => {
                let inner_ty = ty.to_tokens();
                let inner_arm = ty.to_match_arm();
                quote! {
                    oters::export::Value::#head(vs) => {
                        let vec: Vec<#inner_ty> = vs
                            .into_iter()
                            .map(|v| match *v {
                                #inner_arm,
                                _ => unreachable!(),
                            }).collect();
                        vec
                    }
                }
            }
            Tuple(tys) => {
                let indices = 0..tys.len();
                let tys_arms = tys.into_iter().map(|t| t.to_match_arm());
                quote! {
                    oters::export::Value::#head(vs) => (#(match *vs[#indices].clone() {
                        #tys_arms,
                        _ => unreachable!()
                    }),*)
                }
            }
        }
    }

    fn to_type(&self) -> proc_macro2::TokenStream {
        let path = quote!(oters::types::Type);
        use ValueType::*;
        match self {
            Unit => quote!(#path::Unit),
            Int => quote!(#path::Int),
            Float => quote!(#path::Float),
            Bool => quote!(#path::Bool),
            String => quote!(#path::String),
            List(t) => {
                let t = t.to_type();
                quote!(#path::List(std::boxed::Box::new(#t)))
            }
            Tuple(ts) => {
                let ts: Vec<proc_macro2::TokenStream> = ts
                    .into_iter()
                    .map(|t| {
                        let t = t.to_type();
                        quote!(std::boxed::Box::new(#t))
                    })
                    .collect();

                quote!(#path::Tuple(vec![#(#ts),*]))
            }
        }
    }
}
