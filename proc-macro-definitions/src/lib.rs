#![doc(html_root_url = "https://docs.rs/rhizome_proc-macro-definitions/0.0.1")]
#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![allow(clippy::semicolon_if_nothing_returned, clippy::too_many_lines)]

extern crate proc_macro;

use call2_for_syn::call2_strict;
use extractable_declaration::ExtractableDeclaration;
use lazy_static::lazy_static;
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{quote_spanned, ToTokens};
use std::ops::Deref;
use syn::{
	parenthesized,
	parse::{Parse, ParseStream},
	parse_macro_input,
	punctuated::Punctuated,
	Attribute, Error, Generics, Ident, Item, Lifetime, PredicateType, Result, Token, Type,
	TypeParamBound, TypePath, WhereClause, WherePredicate,
};
use tap::Pipe;

mod extractable_declaration;

/// Automatically implements `TypeKey` for an enum, struct, trait, trait alias, type alias or union.
///
/// The implementation is limited by `where Self: 'static`.
///
/// No value is provided.
#[proc_macro_derive(TypeKey)]
pub fn type_key_derive(input: TokenStream1) -> TokenStream1 {
	let rhizome = rhizome_ident(Span::mixed_site());
	let derive_target = parse_macro_input!(input as DeriveTarget);
	implement_type_key(&derive_target, &rhizome, true).into()
}

/// Automatically implements `Dyncast` for an enum, struct, trait, trait alias, type alias or union.
///
/// The implementation is limited to targets that are `'static`,
/// and targeting `Self` is explicit.
///
/// No target is available by default.
#[proc_macro_derive(Dyncast, attributes(dyncast, dyncast))]
pub fn dyncast_derive(input: TokenStream1) -> TokenStream1 {
	let rhizome = rhizome_ident(Span::mixed_site());
	let derive_target = parse_macro_input!(input as DeriveTarget);
	implement_dyncast(&derive_target, &rhizome, true).into()
}

fn implement_type_key(
	impl_target: &ImplTarget,
	rhizome: &Ident,
	ignore_unrelated_attributes: bool,
) -> TokenStream2 {
	let ImplTarget {
		attributes,
		dyn_,
		ident,
		generics,
	} = impl_target;

	let attribute_errors = if ignore_unrelated_attributes {
		vec![]
	} else {
		attributes
			.iter()
			.map(|attribute| {
				Error::new_spanned(attribute, "Attributes are not allowed here.")
					.into_compile_error()
			})
			.collect()
	};

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
		#(#attribute_errors)*

		/// No auto-provision.
		impl#impl_generics ::#rhizome::sync::TypeKey for #dyn_ #ident#type_generics
			#where_clause
		{
			type Error = ::std::convert::Infallible;
		}
	}
}

fn implement_dyncast(
	impl_target: &ImplTarget,
	rhizome: &Ident,
	ignore_unrelated_attributes: bool,
) -> TokenStream2 {
	let ImplTarget {
		attributes,
		dyn_,
		ident,
		generics,
	} = impl_target;

	let mut attribute_errors = if ignore_unrelated_attributes {
		vec![]
	} else {
		attributes
			.iter()
			.map(|attribute| {
				Error::new_spanned(attribute, "Attributes are not allowed here.").to_compile_error()
			})
			.collect()
	};

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

	let targets = attributes
		.iter()
		.filter(|attribute| {
			attribute.path.is_ident("dyncast")
				|| attribute.path.leading_colon.is_some()
					&& attribute.path.segments.len() == 2
					&& attribute.path.segments[0].ident == "rhizome"
					&& attribute.path.segments[1].ident == "dyncast"
		})
		.flat_map(|attribute| {
			for segment in &attribute.path.segments {
				if !segment.arguments.is_empty() {
					attribute_errors.push(
						Error::new_spanned(&segment.arguments, "Unexpected path arguments.")
							.into_compile_error(),
					)
				}
			}

			//FIXME: Should accept comma-separated types.
			call2_strict(attribute.tokens.clone(), |input| {
				let contents;
				parenthesized!(contents in input);
				let targets = Punctuated::<TypePath, Token![,]>::parse_terminated(&contents)?;
				Ok(targets)
			})
		})
		.collect::<Result<Vec<_>>>()
		.unwrap(/*FIXME: Fail better! */);

	quote_spanned! {Span::mixed_site()=>
		#(#attribute_errors)*

		/// # Targets
		///
		#(#[doc = concat!("- `", stringify!(#targets), "`")])*
		impl#impl_generics ::#rhizome::Dyncast for #dyn_ #ident#type_generics
			#where_clause
		{
			fn dyncast<T: 'static + ?Sized>(
				&self
			) -> ::core::option::Option<&T> {
				#(if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<#targets>() {
					::core::option::Option::Some(unsafe {
						(
							&(self as &#targets)
							as *const &#targets
							as *const &T
						).read()
					})
				} else)* {
					None
				}
			}

			fn dyncast_mut<T: 'static + ?Sized>(
				&mut self
			) -> ::core::option::Option<&mut T> {
				#(if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<#targets>() {
					::core::option::Option::Some(unsafe {
						(
							&(self as &mut #targets)
							as *const &mut #targets
							as *const &mut T
						).read()
					})
				} else)* {
					None
				}
			}

			fn dyncast_pinned<'a, T: 'static + ?Sized>(
				self: ::core::pin::Pin<&'a Self>
			) -> ::core::option::Option<::core::pin::Pin<&'a T>> {
				#(if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<#targets>() {
					::core::option::Option::Some(unsafe {
						(
							&(self as ::core::pin::Pin<&#targets>)
							as *const ::core::pin::Pin<&#targets>
							as *const ::core::pin::Pin<&T>
						).read()
					})
				} else)* {
					None
				}
			}

			fn dyncast_pinned_mut<'a, T: 'static + ?Sized>(
				self: ::core::pin::Pin<&'a mut Self>
			) -> ::core::option::Option<::core::pin::Pin<&'a mut T>> {
				#(if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<#targets>() {
					::core::option::Option::Some(unsafe {
						(
							&(self as ::core::pin::Pin<&mut #targets>)
							as *const ::core::pin::Pin<&mut #targets>
							as *const ::core::pin::Pin<&mut T>
						).read()
					})
				} else)* {
					None
				}
			}
		}
	}
}

/// Automatically implements `TypeKey` for an enum, struct, trait, trail alias, type alias or union.
///
/// The implementation is limited by `where Self: 'static`.
///
/// No value is provided.
///
/// # Syntax
///
/// `dyn Ident<Generics: Constraints> where Where: Clause`, `,`-separated.
///
/// - `dyn` is optional,
/// - generics are optional,
/// - a where clause is optional.
#[proc_macro]
pub fn implement_type_keys(input: TokenStream1) -> TokenStream1 {
	let rhizome = rhizome_ident(Span::mixed_site());
	let mut output = TokenStream2::new();
	for impl_target in parse_macro_input!(input as ImplTargets) {
		output.extend(implement_type_key(&impl_target, &rhizome, false));
	}
	output.into()
}

/// Automatically implements `Dyncast` for an enum, struct, trait, trail alias, type alias or union.
///
/// The implementation is limited to targets that are `'static`,
/// and targeting `Self` is explicit.
///
/// No target is available by default.
///
/// # Syntax
///
/// `dyn Ident<Generics: Constraints> where Where: Clause`, `,`-separated.
///
/// - `dyn` is optional,
/// - generics are optional,
/// - a where clause is optional.
#[proc_macro]
pub fn implement_dyncasts(input: TokenStream1) -> TokenStream1 {
	let rhizome = rhizome_ident(Span::mixed_site());
	let mut output = TokenStream2::new();
	for impl_target in parse_macro_input!(input as ImplTargets) {
		output.extend(implement_dyncast(&impl_target, &rhizome, false));
	}
	output.into()
}

struct ImplTarget {
	attributes: Vec<Attribute>,
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
				attributes: enum_.attrs,
				dyn_: None,
				ident: enum_.ident,
				generics: enum_.generics,
			},
			Item::Struct(struct_) => ImplTarget {
				attributes: struct_.attrs,
				dyn_: None,
				ident: struct_.ident,
				generics: struct_.generics,
			},
			Item::Trait(trait_) => ImplTarget {
				attributes: trait_.attrs,
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_.ident,
				generics: trait_.generics,
			},
			Item::TraitAlias(trait_alias) => ImplTarget {
				attributes: trait_alias.attrs,
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_alias.ident,
				generics: trait_alias.generics,
			},
			Item::Type(type_) => ImplTarget {
				attributes: type_.attrs,
				dyn_: None,
				ident: type_.ident,
				generics: type_.generics,
			},
			Item::Union(union) => ImplTarget {
				attributes: union.attrs,
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

//FIXME: This one should also accept type paths.
impl Parse for ImplTarget {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(Self {
			attributes: Attribute::parse_outer(input)?,
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
