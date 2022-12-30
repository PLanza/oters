use proc_macro::{Span, TokenStream};
use quote::{quote, spanned::Spanned};
use syn::{parse_macro_input, FnArg, Ident, ItemFn, ReturnType};

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

                val_types.push(val_ty.to_ident());
            }
            _ => {
                return syn::Error::new(arg.__span(), "Cannot export methods")
                    .to_compile_error()
                    .into()
            }
        }
    }

    // Get the return type as a ValueType
    let val_return = match return_type {
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

    // The indices for the function arguments
    let indices: Vec<usize> = (0..arg_names.len()).collect();

    let mut stmts = fn_body.stmts;
    // Convert the return statement to a Value
    let return_stmt = match stmts[stmts.len() - 1].clone() {
        syn::Stmt::Expr(e) => {
            stmts.pop();
            let ret_ty = val_return.to_ident();
            quote!(crate::oters::export::Value::#ret_ty(#e))
        }
        _ => quote!(crate::oters::export::Value::Unit),
    };

    // Form the exportable function
    let exportable = quote! {
        fn #fn_name(args: Vec<oters::export::Value>) -> oters::export::Value {
            #(let #arg_names = match args[#indices] {
                oters::export::Value::#val_types(v) => v,
                _ => unreachable!()
            };)*
            #(#stmts)*
            #return_stmt
        }
    };

    println!("{}", exportable);

    TokenStream::from(exportable)
}

fn to_val_type(ty: syn::Type) -> Option<ValueType> {
    Some(match ty {
        syn::Type::Path(ty) => {
            if ty.path.get_ident().is_none() {
                return None;
            }
            match ty.path.get_ident().unwrap().to_string().as_str() {
                "bool" => ValueType::Bool,
                "i64" => ValueType::Int,
                "f64" => ValueType::Float,
                "String" => ValueType::String,
                _ => return None,
            }
        }
        _ => todo!(),
    })
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
}
