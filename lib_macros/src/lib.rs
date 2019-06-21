extern crate proc_macro;

use crate::proc_macro::TokenStream;
use quote::quote;
use syn;
#[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]

fn impl_ast_node(ast: &syn::DeriveInput) -> TokenStream {
    if let syn::Data::Struct(struct_) = &ast.data {
        let name = &ast.ident;

        let mut gen = quote! {
            impl AstNode for #name {
                fn syntax(&self) -> NodeId {
                    self.syntax
                }
            }

            impl #name {
                pub fn new(root: NodeId) -> Self {
                    #name {
                        syntax: root,
                        ..Default::default()
                    }
                }
            }
        };

        for field in struct_.fields.iter() {
            match &field.ty {
                syn::Type::Path(type_path) => {
                    let path = &type_path.path;
                    if !path.segments.is_empty() && path.segments[0].ident == "NodeRef" {

                        if let Some(ident) = &field.ident {
                            let getter_str = format!("get_{}", ident);
                            let getter_name = syn::Ident::new(&getter_str, ident.span());
                            let setter_str = format!("set_{}", ident);
                            let setter_name = syn::Ident::new(&setter_str, ident.span());
                            let ty = &field.ty;
                            let error_msg = format!("{} is already set.", ident);

                            let field = quote! {
                                impl #name {
                                    pub fn #getter_name(&self) -> &#ty {
                                        &self.#ident
                                    }

                                    pub fn #setter_name(&mut self, value: NodeId) {
                                        if !self.#ident.is_none() {
                                            panic!(#error_msg)
                                        }
                                        self.#ident = Some(value);
                                    }
                                }
                            };

                            gen = quote! {
                                #gen
                                #field
                            };
                        }
                    }
                }
                _ => {}
            };
        }

        let res = gen.into();
        res
    } else {
        //Nope. This is an Enum. We cannot handle these!
        panic!("#[derive(AstNodeDefault)] is only defined for structs, not for enums!");
    }
}

#[proc_macro_derive(AstNode)]
pub fn ast_node_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_ast_node(&ast)
}
