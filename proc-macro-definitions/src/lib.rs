#![doc(html_root_url = "https://docs.rs/rhizome_proc-macro-definitions/0.0.1")]
#![forbid(unsafe_code)]

extern crate proc_macro;

mod extractable_declaration;

use {
    extractable_declaration::ExtractableDeclaration,
    lazy_static::lazy_static,
    proc_macro::TokenStream as TokenStream1,
    proc_macro2::{Span, TokenStream as TokenStream2},
    proc_macro_crate::crate_name,
    syn::{parse_macro_input, Ident},
};

#[proc_macro]
pub fn extractable(input: TokenStream1) -> TokenStream1 {
    let extractable_declaration = parse_macro_input!(input as ExtractableDeclaration);
    let tokens: TokenStream2 = extractable_declaration
        .into_tokens()
        .unwrap_or_else(|error| error.to_compile_error());
    tokens.into()
}

lazy_static! {
    static ref RHIZOME_NAME: String =
        crate_name("rhizome").unwrap_or_else(|_| "rhizome".to_owned());
}
fn rhizome_ident(span: Span) -> Ident {
    Ident::new(&*RHIZOME_NAME, span)
}
