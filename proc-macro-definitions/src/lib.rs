#![doc(html_root_url = "https://docs.rs/rhizome_proc-macro-definitions/0.0.1")]
#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![allow(clippy::too_many_lines)]

extern crate proc_macro;

mod extractable_declaration;

use extractable_declaration::ExtractableDeclaration;
use lazy_static::lazy_static;
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::quote_spanned;
use syn::{
	parse::{Parse, ParseStream},
	parse_macro_input, Error, Generics, Ident, Item, Result, Token,
};
use tap::Pipe;

#[proc_macro_derive(TypeKey)]
pub fn type_key_derive(input: TokenStream1) -> TokenStream1 {
	let rhizome = rhizome_ident(Span::mixed_site());
	let DeriveTarget {
		dyn_,
		ident,
		generics,
	} = parse_macro_input!(input as DeriveTarget);
	let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
	(quote_spanned! {Span::mixed_site()=>
		/// No auto-provision.
		impl#impl_generics ::#rhizome::sync::TypeKey for #dyn_ #ident#type_generics
			#where_clause
		{
			type Error = ::std::convert::Infallible;
		}
	})
	.into()
}

struct DeriveTarget {
	dyn_: Option<Token![dyn]>,
	ident: Ident,
	generics: Generics,
}

impl Parse for DeriveTarget {
	fn parse(input: ParseStream) -> Result<Self> {
		let item: Item = input.parse()?;
		match item {
			Item::Enum(enum_) => Self {
				dyn_: None,
				ident: enum_.ident,
				generics: enum_.generics,
			},
			Item::Struct(struct_) => Self {
				dyn_: None,
				ident: struct_.ident,
				generics: struct_.generics,
			},
			Item::Trait(trait_) => Self {
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_.ident,
				generics: trait_.generics,
			},
			Item::TraitAlias(trait_alias) => Self {
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_alias.ident,
				generics: trait_alias.generics,
			},
			Item::Type(type_) => Self {
				dyn_: None,
				ident: type_.ident,
				generics: type_.generics,
			},
			Item::Union(union) => Self {
				dyn_: None,
				ident: union.ident,
				generics: union.generics,
			},
			_ => return Err(Error::new(
				Span::mixed_site(),
				"`TypeKey` can only be derived for structs, enums, unions, traits, trait aliases and type aliases.",
			)),
		}
		.pipe(Ok)
	}
}

#[proc_macro]
pub fn extractable(input: TokenStream1) -> TokenStream1 {
	let extractable_declaration = parse_macro_input!(input as ExtractableDeclaration);
	let tokens: TokenStream2 = extractable_declaration.into_tokens();
	tokens.into()
}

lazy_static! {
	static ref RHIZOME_NAME: String = match crate_name("rhizome") {
		Ok(FoundCrate::Name(name)) => name,
		Ok(FoundCrate::Itself) | Err(_) => "rhizome".to_owned(),
	};
}
fn rhizome_ident(span: Span) -> Ident {
	Ident::new(&*RHIZOME_NAME, span)
}
