use proc_macro::Span;
use quote::quote;
use syn::Ident;

#[derive(Debug, Clone)]
pub(crate) enum ValueType {
    Bool,
    Int,
    Float,
    String,
    Unit,
    List(Box<ValueType>),
    Tuple(Vec<Box<ValueType>>),
    // Fn
    Struct(String),
    // Enum
}

impl ValueType {
    pub(crate) fn from_syn_type(ty: syn::Type) -> Option<ValueType> {
        Some(match ty {
            syn::Type::Path(ty) => {
                if ty.path.get_ident().is_none() {
                    let ty = ty.path.segments.last().unwrap();
                    if ty.ident != "Vec" {
                        if crate::EXPORT_STRUCTS
                            .lock()
                            .unwrap()
                            .contains_key(&ty.ident.to_string())
                        {
                            return Some(ValueType::Struct(ty.ident.to_string()));
                        }
                        return None;
                    }
                    let inner_ty = match &ty.arguments {
                        syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments { args, .. },
                        ) => match &args[0] {
                            syn::GenericArgument::Type(inner) => {
                                ValueType::from_syn_type(inner.clone())?
                            }
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
                    ident => {
                        if crate::EXPORT_STRUCTS
                            .lock()
                            .unwrap()
                            .contains_key(&ident.to_string())
                        {
                            ValueType::Struct(ident.to_string())
                        } else {
                            return None;
                        }
                    }
                }
            }
            syn::Type::Tuple(syn::TypeTuple { elems, .. }) => {
                let mut types = Vec::new();
                for ty in elems {
                    types.push(Box::new(ValueType::from_syn_type(ty)?));
                }
                ValueType::Tuple(types)
            }
            _ => return None,
        })
    }

    pub(crate) fn to_ident(&self) -> Ident {
        use ValueType::*;
        match self {
            Unit => Ident::new("Unit", Span::call_site().into()),
            Bool => Ident::new("Bool", Span::call_site().into()),
            Int => Ident::new("Int", Span::call_site().into()),
            Float => Ident::new("Float", Span::call_site().into()),
            String => Ident::new("String", Span::call_site().into()),
            List(_) => Ident::new("List", Span::call_site().into()),
            Tuple(_) => Ident::new("Tuple", Span::call_site().into()),
            Struct(_) => Ident::new("Struct", Span::call_site().into()),
        }
    }

    pub(crate) fn to_tokens(&self) -> proc_macro2::TokenStream {
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
            Struct(s) => {
                let strct = Ident::new(&s, Span::call_site().into());
                quote!(#strct)
            }
        }
    }

    pub(crate) fn to_match_arm(&self) -> proc_macro2::TokenStream {
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
            Struct(name) => match crate::EXPORT_STRUCTS.lock().unwrap().get(name) {
                Some(fields) => {
                    let name = Ident::new(name, Span::call_site().into());
                    let mut field_idents = Vec::new();
                    let mut field_strs = Vec::new();
                    let mut field_arms = Vec::new();
                    for (field, val) in fields {
                        field_idents.push(Ident::new(field, Span::call_site().into()));
                        field_strs.push(field);
                        field_arms.push(val.to_match_arm());
                    }
                    quote! {
                        oters::export::Value::Struct(_, map) => #name {
                            #(#field_idents: match *map.get(#field_strs).unwrap().clone() {
                                #field_arms,
                                _ => unreachable!()
                            }),* }
                    }
                }
                None => syn::Error::new(
                    proc_macro::Span::call_site().into(),
                    format!("Struct {} has not been exported", name),
                )
                .to_compile_error()
                .into(),
            },
        }
    }

    pub(crate) fn to_type(&self) -> proc_macro2::TokenStream {
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
            Struct(name) => {
                let map = crate::EXPORT_STRUCTS
                    .lock()
                    .unwrap()
                    .get(name)
                    .unwrap()
                    .clone();

                let map_tokens: Vec<proc_macro2::TokenStream> = map
                    .into_iter()
                    .map(|(f, t)| {
                        let t = t.to_type();
                        quote!((#f.to_string(), std::boxed::Box::new(#t)))
                    })
                    .collect();

                quote!(#path::Struct(std::collections::HashMap::from([#(#map_tokens),*])))
            }
        }
    }
}
