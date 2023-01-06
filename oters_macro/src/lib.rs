mod types;

use std::{collections::HashMap, sync::Mutex};

use crate::types::ValueType;

use lazy_static::lazy_static;
use proc_macro::{Span, TokenStream};
use quote::{quote, spanned::Spanned};
use syn::{FnArg, Ident, ReturnType};

lazy_static! {
    static ref EXPORT_FNS: Mutex<HashMap<String, (Vec<ValueType>, ValueType)>> =
        Mutex::new(HashMap::new());
    static ref EXPORT_STRUCTS: Mutex<HashMap<String, HashMap<String, ValueType>>> =
        Mutex::new(HashMap::new());
}

#[proc_macro_attribute]
pub fn export_oters(_args: TokenStream, item: TokenStream) -> TokenStream {
    match syn::parse::<syn::ItemFn>(item.clone()) {
        Ok(item) => export_fn(item),
        Err(_) => match syn::parse::<syn::ItemStruct>(item.clone()) {
            Ok(item) => export_struct(item),
            Err(_) => {
                return syn::Error::new(
                    proc_macro2::TokenStream::from(item).__span(),
                    "Can only export function and struct declarations",
                )
                .to_compile_error()
                .into()
            }
        },
    }
}

fn export_fn(item: syn::ItemFn) -> TokenStream {
    let fn_name = item.sig.ident;
    let args: Vec<FnArg> = item.sig.inputs.iter().cloned().collect();
    let return_type = item.sig.output;
    let fn_body = *item.block;

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
                let val_ty = match ValueType::from_syn_type(*ty.clone()) {
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
        ReturnType::Type(_, ty) => match ValueType::from_syn_type(*ty.clone()) {
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
    EXPORT_FNS.lock().unwrap().insert(map_entry.0, map_entry.1);

    TokenStream::from(exportable)
}

fn export_struct(item: syn::ItemStruct) -> TokenStream {
    let clone = item.clone();
    let struct_name = item.ident;
    if item.generics.params.len() != 0 {
        return syn::Error::new(
            item.generics.params.__span(),
            "Cannot export generic struct",
        )
        .to_compile_error()
        .into();
    }

    let fields = match item.fields {
        syn::Fields::Named(syn::FieldsNamed { named, .. }) => named,
        _ => {
            return syn::Error::new(item.fields.__span(), "struct fields must be named")
                .to_compile_error()
                .into()
        }
    };

    let mut to_export = HashMap::new();
    for syn::Field { ident, ty, .. } in fields {
        to_export.insert(
            ident.unwrap().to_string(),
            ValueType::from_syn_type(ty).unwrap(),
        );
    }

    EXPORT_STRUCTS
        .lock()
        .unwrap()
        .insert(struct_name.to_string(), to_export);

    quote!(#clone).into()
}
// Called after all #[export_oters]s and puts all the exported items into hashmaps
#[proc_macro]
pub fn export_list(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let path = quote!(oters::types::Type);

    let fns_map = EXPORT_FNS.lock().unwrap().clone();

    // Using .map() instead of for loop because the for loop was buggy
    let functions: Vec<(
        String,
        Ident,
        proc_macro2::TokenStream,
        proc_macro2::TokenStream,
    )> = fns_map
        .into_iter()
        .map(|(name, (args, ret_val))| {
            let ptr = Ident::new(&name, Span::call_site().into());

            let ret_type = ret_val.to_type();
            let arg_types = if args.len() == 0 {
                quote!(vec![#path::Unit])
            } else {
                // Multiple arguments get converted to tuples to prevent partial application
                let arg_tys = args.into_iter().map(|arg| arg.to_type());
                quote!(vec![#(#arg_tys),*])
            };

            (name.to_string(), ptr, arg_types, ret_type)
        })
        .collect();

    let fn_names: Vec<String> = functions.clone().into_iter().map(|tuple| tuple.0).collect();
    let fn_pointers: Vec<Ident> = functions.clone().into_iter().map(|tuple| tuple.1).collect();
    let arg_types: Vec<proc_macro2::TokenStream> =
        functions.clone().into_iter().map(|tuple| tuple.2).collect();
    let ret_types: Vec<proc_macro2::TokenStream> =
        functions.into_iter().map(|tuple| tuple.3).collect();

    let structs_map = EXPORT_STRUCTS.lock().unwrap().clone();
    let structs: Vec<(String, proc_macro2::TokenStream)> = structs_map
        .into_iter()
        .map(|(name, fields)| {
            let fields: Vec<(String, proc_macro2::TokenStream)> = fields
                .into_iter()
                .map(|(field, ty)| (field, ty.to_type()))
                .collect();
            let field_names: Vec<String> = fields.clone().into_iter().map(|pair| pair.0).collect();
            let field_tys: Vec<proc_macro2::TokenStream> =
                fields.into_iter().map(|pair| pair.1).collect();

            (
                name,
                quote!(HashMap::from([#((#field_names.to_string(), std::boxed::Box::new(#field_tys))),*])),
            )
        })
        .collect();
    let struct_names: Vec<String> = structs.clone().into_iter().map(|pair| pair.0).collect();
    let struct_maps: Vec<proc_macro2::TokenStream> =
        structs.into_iter().map(|pair| pair.1).collect();

    let out = quote! {
        use std::collections::HashMap;
        use lazy_static::lazy_static;
        lazy_static! {
            static ref EXPORT_FNS: oters::export::ExportFns =
                HashMap::from([#(
                        (#fn_names.to_string(),
                         (#fn_pointers as fn(Vec<oters::export::Value>) -> oters::export::Value,
                          #arg_types,
                          #ret_types)
                         )
                        ),*]);
            static ref EXPORT_STRUCTS: Vec<(String, HashMap<String, Box<oters::types::Type>>)> =
                vec![#((#struct_names.to_string(), #struct_maps)),*];
        }
    };

    println!("{}", out);

    out.into()
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
                .collect::<Vec<std::boxed::Box<oters::export::Value>>>()
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
        ValueType::Unit => {
            quote! {
                #e;
                oters::export::Value::Unit
            }
        }
        _ => quote!(oters::export::Value::#ret_ty(#e)),
    }
}
