use crate::rhizome_ident;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
	parenthesized,
	parse::{Parse, ParseStream, Result},
	parse_quote,
	punctuated::Punctuated,
	spanned::Spanned,
	Attribute, Error, Ident, ReturnType, Token, TraitItemMethod, Type, TypePath, Visibility,
};
use syn_mid::{ItemFn, Signature};

mod kw {
	use syn::custom_keyword;
	custom_keyword!(Debug);
	custom_keyword!(Send);
	custom_keyword!(Sync);
}

pub struct ExtractableDeclaration {
	attributes: Vec<Attribute>,
	visibility: Visibility,
	abstract_token: Option<Token![abstract]>,
	name: Ident,
	debug: Option<Span>,
	send: Option<Span>,
	sync: Option<Span>,
	trait_body: Vec<TokenStream>,
	dyn_body: Vec<TokenStream>,
	struct_body: Vec<TokenStream>,
	initializer_body: Vec<TokenStream>,
	impl_body: Vec<TokenStream>,
	provision: Provision,
}

enum Provision {
	Never,
	AtRoot,
	At(TypePath),
}

impl Parse for ExtractableDeclaration {
	fn parse(input: ParseStream<'_>) -> Result<Self> {
		let attributes = input.call(Attribute::parse_outer)?;

		let visibility = input.parse()?;
		let abstract_token = input.parse().ok();
		input.parse::<Token![trait]>()?;
		let name = input.parse()?;

		//HACK
		let mut debug = None;
		let mut send = None;
		let mut sync = None;
		if input.parse::<Token![:]>().is_ok() {
			let mut additional = false;
			if let Ok(d) = input.parse::<kw::Debug>() {
				debug = Some(d.span);
				additional = true;
			}
			if !additional || input.parse::<Token![+]>().is_ok() {
				if let Ok(se) = input.parse::<kw::Send>() {
					send = Some(se.span);
					additional = true;
				}
				if !additional || input.parse::<Token![+]>().is_ok() {
					if let Ok(sy) = input.parse::<kw::Sync>() {
						sync = Some(sy.span);
					}
				}
			}
		}
		let (debug, send, sync) = (debug, send, sync);

		let provision =
			if input.peek(Token![for]) {
				let for_token = input.parse::<Token![for]>()?;
				if abstract_token.is_some() {
					return Err(Error::new_spanned(
						for_token,
						"Default provision is not available for abstract extractables.",
					));
				}

				if input.peek(Token![final]) {
					input.parse::<Token![final]>()?; //TODO: Wording?
					Provision::AtRoot
				} else {
					Provision::At(input.parse().map_err(|error| {
						Error::new(error.span(), "Expected final or type path.")
					})?)
				}
			} else {
				Provision::Never
			};

		let mut trait_body = Vec::new();
		let mut dyn_body = Vec::new();
		let mut struct_body = Vec::new();
		let mut initializer_body = Vec::new();
		let mut impl_body = Vec::new();
		while !input.is_empty() {
			let lookahead = input.lookahead1();
			if lookahead.peek(Token![ref]) {
				let ref_token = input.parse::<Token![ref]>()?;

				if abstract_token.is_some() {
					return Err(Error::new_spanned(
                        ref_token,
                        "References to other extractables are not available in abstract extractables."
                    ));
				}

				let field_name: Ident = input.parse()?;
				let colon: Token![:] = input.parse()?;

				let extractable_type: Type = input.parse()?;
				let question: Token![?] = input.parse()?;

				struct_body.push(quote_spanned! {extractable_type.span()=>
					#field_name#colon std::sync::Arc<#extractable_type>,
				});
				initializer_body.push({
					let rhizome = rhizome_ident(Span::call_site());
					let map_missing_dependency = quote_spanned! {question.span=>
						.map_err(|error| #rhizome::error::AutoProvisionError::MissingDependency(
							core::any::type_name::<#extractable_type>(),
							error
						))
					};
					quote_spanned! {extractable_type.span()=>
						#field_name#colon <#extractable_type>::extract_from(node)#map_missing_dependency#question,
					}
				});
			} else if lookahead.peek(Token![abstract]) {
				let inner_abstract_token = input.parse::<Token![abstract]>()?;
				if abstract_token.is_some() {
					return Err(Error::new_spanned(
						inner_abstract_token,
						"Implementation modifiers are not available in abstract extractables.",
					));
				}

				let async_token: Option<Token![async]> = input.parse().ok();
				let async_refs: Option<Vec<Ident>> = if async_token.is_some() {
					let async_ref_list;
					parenthesized!(async_ref_list in input);
					Some(
						async_ref_list
							.call(Punctuated::<Ident, Token![,]>::parse_terminated)?
							.into_iter()
							.collect(),
					)
				} else {
					None
				};

				let ItemFn {
					attrs,
					vis,
					sig,
					block,
				} = input.parse()?;

				if !attrs.is_empty() {
					return Err(Error::new_spanned(
						quote!(#(#attrs)*),
						"Attributes are not allowed here.
                        Move them before `abstract` instead.",
					));
				}

				if let Some(async_token) = async_token {
					let async_refs = async_refs.unwrap();
					let output = match match sig.output {
						ReturnType::Default => parse_quote!(-> ()),
						explicit @ ReturnType::Type(_, _) => explicit,
					} {
						ReturnType::Type(r_arrow, r_type) => {
							parse_quote!(#r_arrow Box<dyn core::future::Future<Output = #r_type>>)
						}
						ReturnType::Default => unreachable!(),
					};
					let sig = Signature {
						asyncness: None,
						output,
						..sig
					};
					trait_body.push(quote! {
						#vis // This will just make it fail if any explicit visibility is given.
						#sig;
					});
					impl_body.push(quote! {
						#sig {
							#(let #async_refs = self.#async_refs.clone();)*
							Box::new(#async_token move #block)
						}
					});
				} else {
					trait_body.push(quote! {
						#vis // This will just make it fail if any explicit visibility is given.
						#sig;
					});
					impl_body.push(quote! {
						#sig #block
					});
				}
			} else if lookahead.peek(Token![final]) {
				let final_token = input.parse::<Token![final]>()?;
				if abstract_token.is_some() {
					return Err(Error::new_spanned(
						final_token,
						"Implementation modifiers are not available in abstract extractables.",
					));
				}

				let item_fn: ItemFn = input.parse()?;
				if !item_fn.attrs.is_empty() {
					let attrs = item_fn.attrs;
					return Err(Error::new_spanned(
						quote!(#(#attrs)*),
						"Attributes are not allowed here.
                        Move them before `final` instead.",
					));
				}

				match item_fn.vis {
					Visibility::Inherited => (),
					vis => {
						return Err(Error::new_spanned(
							vis,
							"Trait object functions are implicitly public.",
						))
					}
				}

				dyn_body.push(quote! {
					pub #item_fn
				});
			} else if abstract_token.is_some() && lookahead.peek(Token![fn]) {
				let method: TraitItemMethod = input.parse()?;
				//TODO: Support async transformation.
				trait_body.push(method.into_token_stream());
			} else {
				return Err(lookahead.error());
			}
		}

		Ok(Self {
			attributes,
			visibility,
			abstract_token,
			name,
			debug,
			send,
			sync,
			trait_body,
			dyn_body,
			struct_body,
			initializer_body,
			impl_body,
			provision,
		})
	}
}

impl ExtractableDeclaration {
	pub fn into_tokens(self) -> TokenStream {
		let Self {
			attributes,
			visibility,
			abstract_token,
			name,
			debug,
			send,
			sync,
			trait_body,
			dyn_body,
			struct_body,
			initializer_body,
			impl_body,
			provision,
		} = self;

		let rhizome = rhizome_ident(Span::call_site());
		let name_impl = Ident::new(&format!("{}Impl", name), name.span());

		let (debug, derive_debug) = debug.map_or((None, None), |span| {
			(
				Some(quote_spanned!(span=> + ::core::fmt::Debug)),
				Some(quote_spanned!(span=> #[derive(Debug)])),
			)
		});
		let send = send.map(|span| Some(quote_spanned!(span=> + ::core::marker::Send)));
		let sync = sync.map(|span| Some(quote_spanned!(span=> + ::core::marker::Sync)));

		let default_provide = if abstract_token.is_some() {
			None
		} else {
			Some(quote! {
				pub fn provide(node: &mut #rhizome::Node) -> Result<(), #rhizome::error::AutoProvisionError> {
					let dyn_arc: std::sync::Arc<Self> = std::sync::Arc::new(#name_impl::new(node)?);
					node.provide(
						<#name_impl as #rhizome::TypeKey<#rhizome::error::AutoProvisionError>>::key(),
						Box::new(dyn_arc)
					).map_err(|_| #rhizome::error::AutoProvisionError::KeyExists)
				}
			})
		};

		let default_impl = if abstract_token.is_some() {
			None
		} else {
			Some(quote! {
				impl #name_impl {
					pub fn new(node: &#rhizome::Node) -> Result<Self, #rhizome::error::AutoProvisionError> {
						Ok(Self {
							#(#initializer_body)*
						})
					}

					fn factory(node: &#rhizome::Node) -> Result<Box<dyn core::any::Any>, #rhizome::error::AutoProvisionError> {
						let dyn_arc: std::sync::Arc<dyn #name> = std::sync::Arc::new(#name_impl::new(node)?);
						Ok(Box::new(dyn_arc))
					}
				}
				impl #name for #name_impl {
					#(#impl_body)*
				}
			})
		};

		let provision = match provision {
			Provision::Never => quote!(#rhizome::Provision::Never),
			Provision::AtRoot => quote!(#rhizome::Provision::at_root(#name_impl::factory)),
			Provision::At(owner) => {
				quote!(#rhizome::Provision::at_owner::<#owner>(#name_impl::factory))
			}
		};

		quote! {
			#(#attributes)*
			#visibility trait #name: core::any::Any #debug #send #sync {
				#(#trait_body)*
			}
			impl dyn #name {
				//TODO?: Encapsulate these functions in a trait so they can't collide with #dyn_body?
				//TODO: Better errors that can show a trace of requirements *by name when possible*.
				pub fn extract_from(node: &#rhizome::Node) -> Result<std::sync::Arc<Self>, #rhizome::error::Error> {
					let any_box_guard = node.extract_with::<#name_impl, _>()?;
					if let Some(extractable_arc) = any_box_guard.downcast_ref::<std::sync::Arc<dyn #name>>() {
						Ok(extractable_arc.clone())
					} else {
						panic!(
							"Found unexpected value for extractable.
                            Expected: {}
                            Found: {:?}
                            This should be impossible (unless you use the extractable's \"Impl\" struct directly, which you shouldn't), so please file a bug report for Rhizome if you run into this panic!",
							core::any::type_name::<std::sync::Arc<Self>>(),
							&**any_box_guard
						);
					}
				}

				#default_provide

				pub fn provide_custom<I: #name>(
					node: &mut #rhizome::Node,
					instance: I,
				) -> Result<(), ()> {
					let dyn_arc: std::sync::Arc<Self> = std::sync::Arc::new(instance);
					node.provide(
						<#name_impl as #rhizome::TypeKey<#rhizome::error::AutoProvisionError>>::key(),
						Box::new(dyn_arc),
					)
					.map_err(|_| ()) //TODO?: Return instance if possible.
				}

				#(#dyn_body)*
			}

			//TODO?: Derive name from #name for better error messages/Debug.
			#derive_debug
			pub struct #name_impl {
				#(#struct_body)*
			}
			#default_impl
			impl #rhizome::TypeKey<#rhizome::error::AutoProvisionError> for #name_impl {
				fn provision() -> #rhizome::Provision<#rhizome::error::AutoProvisionError, Box<dyn core::any::Any>> {
					#provision
				}
			}
		}
	}
}
