#![doc(html_root_url = "https://docs.rs/rhizome_proc-macro-definitions/0.0.1")]
#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![allow(clippy::too_many_lines)]

extern crate proc_macro;

mod extractable_declaration;

use std::ops::Deref;

use extractable_declaration::ExtractableDeclaration;
use lazy_static::lazy_static;
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{quote_spanned, ToTokens};
use syn::{
	parse::{Parse, ParseStream},
	parse_macro_input,
	punctuated::Punctuated,
	Error, Generics, Ident, Item, Lifetime, PredicateType, Result, Token, Type, TypeParamBound,
	WhereClause, WherePredicate,
};
use tap::Pipe;

/// Automatically implements `TypeKey` for an enum, struct, trait, trait alias, type alias or union.
///
/// The implementation is limited by `where Self: 'static`.
///
/// No value is provided.
#[proc_macro_derive(TypeKey)]
pub fn type_key_derive(input: TokenStream1) -> TokenStream1 {
	let rhizome = rhizome_ident(Span::mixed_site());
	let derive_target = parse_macro_input!(input as DeriveTarget);
	implement_type_key(&derive_target, &rhizome).into()
}

fn implement_type_key(impl_target: &ImplTarget, rhizome: &Ident) -> TokenStream2 {
	let ImplTarget {
		dyn_,
		ident,
		generics,
	} = impl_target;

	let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

	let mut where_clause = where_clause.cloned().unwrap_or_else(|| WhereClause {
		where_token: Token![where](Span::mixed_site()),
		predicates: Punctuated::default(),
	});
	let predicates = &mut where_clause.predicates;
	predicates.push(WherePredicate::Type(PredicateType {
		lifetimes: None,
		bounded_ty: Type::Verbatim(Token![Self](Span::mixed_site()).into_token_stream()),
		colon_token: Token![:](Span::mixed_site()),
		bounds: {
			let mut bounds = Punctuated::new();
			bounds.push_value(TypeParamBound::Lifetime(Lifetime {
				apostrophe: Span::mixed_site(),
				ident: Ident::new("static", Span::mixed_site()),
			}));
			bounds
		},
	}));

	quote_spanned! {Span::mixed_site()=>
		/// No auto-provision.
		impl#impl_generics ::#rhizome::sync::TypeKey for #dyn_ #ident#type_generics
			#where_clause
		{
			type Error = ::std::convert::Infallible;
		}
	}
}

/// Automatically implements `TypeKey` for an enum, struct, trait, trail alias, type alias or union.
///
/// The implementation is limited by `where Self: 'static`.
///
/// No value is provided.
///
/// Syntax: `dyn Ident<Generics: Constraints> where Where: Clause`, `,`-separated.
#[proc_macro]
pub fn implement_type_keys(input: TokenStream1) -> TokenStream1 {
	let rhizome = rhizome_ident(Span::mixed_site());
	let mut output = TokenStream2::new();
	for impl_target in parse_macro_input!(input as ImplTargets) {
		output.extend(implement_type_key(&impl_target, &rhizome));
	}
	output.into()
}

struct ImplTarget {
	dyn_: Option<Token![dyn]>,
	ident: Ident,
	generics: Generics,
}

struct DeriveTarget(ImplTarget);
impl Deref for DeriveTarget {
	type Target = ImplTarget;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl Parse for DeriveTarget {
	fn parse(input: ParseStream) -> Result<Self> {
		let item: Item = input.parse()?;
		match item {
			Item::Enum(enum_) => ImplTarget {
				dyn_: None,
				ident: enum_.ident,
				generics: enum_.generics,
			},
			Item::Struct(struct_) => ImplTarget {
				dyn_: None,
				ident: struct_.ident,
				generics: struct_.generics,
			},
			Item::Trait(trait_) => ImplTarget {
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_.ident,
				generics: trait_.generics,
			},
			Item::TraitAlias(trait_alias) => ImplTarget {
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_alias.ident,
				generics: trait_alias.generics,
			},
			Item::Type(type_) => ImplTarget {
				dyn_: None,
				ident: type_.ident,
				generics: type_.generics,
			},
			Item::Union(union) => ImplTarget {
				dyn_: None,
				ident: union.ident,
				generics: union.generics,
			},
			_ => return Err(Error::new(
				Span::mixed_site(),
				"`TypeKey` can only be derived for structs, enums, unions, traits, trait aliases and type aliases.",
			)),
		}.pipe(Self)
		.pipe(Ok)
	}
}

struct ImplTargets(Vec<ImplTarget>);
impl IntoIterator for ImplTargets {
	type Item = ImplTarget;

	type IntoIter = <Vec<ImplTarget> as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter()
	}
}

impl Parse for ImplTargets {
	fn parse(input: ParseStream) -> Result<Self> {
		Punctuated::<ImplTarget, Token![,]>::parse_terminated(input)?
			.into_iter()
			.collect::<Vec<_>>()
			.pipe(Self)
			.pipe(Ok)
	}
}

impl Parse for ImplTarget {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(Self {
			dyn_: input.parse()?,
			ident: input.parse()?,
			generics: {
				let mut generics: Generics = input.parse()?;
				generics.where_clause = input.parse()?;
				generics
			},
		})
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
