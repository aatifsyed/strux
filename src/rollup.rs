use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Attribute, Expr, Ident, Token, Type, Visibility, braced, parenthesized,
    parse::{Nothing, Parse, ParseStream},
    punctuated::{Pair, Punctuated},
    token,
};

pub fn _rollup(input: ParseStream) -> syn::Result<TokenStream> {
    Ok(generate(input.parse()?))
}

fn generate(Ast { first, items }: Ast) -> TokenStream {
    let mut output = TokenStream::new();
    let (mut running_ident, mut running_fields) = match first {
        First::Extern(ExternStruct {
            r#extern: _,
            r#struct: _,
            ident,
            brace: _,
            fields,
        }) => (ident, fields),
        First::Struct(first) => {
            first.to_tokens(&mut output);
            let Struct { ident, fields, .. } = first;
            (ident, fields)
        }
    };

    for item in items {
        let mut r#struct = match item {
            Item::Linked {
                conv:
                    Conv {
                        attrs,
                        vis,
                        r#fn,
                        ident: fn_name,
                        paren: _,
                        rcvr,
                        semi: _,
                    },
                r#struct,
            } => {
                let running_fields = running_fields
                    .iter()
                    .map(|field| &field.ident)
                    .collect::<Vec<_>>();
                let new_fields = r#struct.fields.iter().map(
                    |NamedField {
                         ident,
                         make: Make { expr, .. },
                         ..
                     }| quote!(#ident: #expr),
                );
                let struct_name = &r#struct.ident;
                output.extend(match rcvr {
                    Some(sel) => quote! {
                        impl #running_ident {
                            #(#attrs)*
                            #vis #r#fn #fn_name(#sel) -> #struct_name {
                                let #running_ident {
                                    #(#running_fields,)*
                                } = self;
                                #struct_name {
                                    #(#running_fields,)*
                                    #(#new_fields,)*
                                }
                            }
                        }
                    },
                    None => quote! {
                        #(#attrs)*
                        #vis #r#fn #fn_name(#running_ident {
                            #(#running_fields,)*
                        }: #running_ident) -> #struct_name {
                            #struct_name {
                                #(#running_fields,)*
                                #(#new_fields,)*
                            }
                        }
                    },
                });
                r#struct.map(|field| field.map(|_| Nothing))
            }
            Item::Unlinked(r#struct) => r#struct,
        };
        running_fields.extend(r#struct.fields);
        r#struct.fields = running_fields;

        r#struct.to_tokens(&mut output);

        running_fields = r#struct.fields;
        running_ident = r#struct.ident;
    }

    output
}

struct Ast {
    first: First,
    items: Vec<Item>,
}

impl Parse for Ast {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let r#first = match input.peek(Token![extern]) {
            true => {
                let content;
                First::Extern(ExternStruct {
                    r#extern: input.parse()?,
                    r#struct: input.parse()?,
                    ident: input.parse()?,
                    brace: braced!(content in input),
                    fields: content.call(Punctuated::parse_terminated)?,
                })
            }
            false => First::Struct({
                let content;
                Struct {
                    attrs: input.call(Attribute::parse_outer)?,
                    vis: input.parse()?,
                    r#struct: input.parse()?,
                    ident: input.parse()?,
                    brace: braced!(content in input),
                    fields: content.call(Punctuated::parse_terminated)?,
                }
            }),
        };

        let mut items = vec![];

        while !input.is_empty() {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis = input.parse()?;
            let item = match input.peek(Token![fn]) {
                true => Item::Linked {
                    conv: {
                        let content;
                        Conv {
                            attrs,
                            vis,
                            r#fn: input.parse()?,
                            ident: input.parse()?,
                            paren: parenthesized!(content in input),
                            rcvr: content.parse()?,
                            semi: input.parse()?,
                        }
                    },
                    r#struct: {
                        let content;
                        Struct {
                            attrs: input.call(Attribute::parse_outer)?,
                            vis: input.parse()?,
                            r#struct: input.parse()?,
                            ident: input.parse()?,
                            brace: braced!(content in input),
                            fields: content.call(Punctuated::parse_terminated)?,
                        }
                    },
                },
                false => Item::Unlinked({
                    let content;
                    Struct {
                        attrs,
                        vis,
                        r#struct: input.parse()?,
                        ident: input.parse()?,
                        brace: braced!(content in input),
                        fields: content.call(Punctuated::parse_terminated)?,
                    }
                }),
            };

            items.push(item);
        }

        Ok(Ast { first, items })
    }
}

enum First {
    Extern(ExternStruct),
    Struct(Struct<NamedField<Nothing>>),
}

enum Item {
    Linked {
        conv: Conv,
        r#struct: Struct<NamedField<Make>>,
    },
    Unlinked(Struct<NamedField<Nothing>>),
}

struct ExternStruct {
    #[expect(dead_code)]
    r#extern: Token![extern],
    #[expect(dead_code)]
    r#struct: Token![struct],
    ident: Ident,
    #[expect(dead_code)]
    brace: token::Brace,
    fields: Punctuated<NamedField<Nothing>, Token![,]>,
}

struct Conv {
    attrs: Vec<Attribute>,
    vis: Visibility,
    r#fn: Token![fn],
    ident: Ident,
    #[expect(dead_code)]
    paren: token::Paren,
    rcvr: Option<Token![self]>,
    #[expect(dead_code)]
    semi: Token![;],
}

struct Struct<T> {
    attrs: Vec<Attribute>,
    vis: Visibility,
    r#struct: Token![struct],
    ident: Ident,
    brace: token::Brace,
    fields: Punctuated<T, Token![,]>,
}

impl<T> Struct<T> {
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Struct<U> {
        let Self {
            attrs,
            vis,
            r#struct,
            ident,
            brace,
            fields,
        } = self;
        Struct {
            attrs,
            vis,
            r#struct,
            ident,
            brace,
            fields: fields
                .into_pairs()
                .map(|pair| {
                    let (item, punct) = pair.into_tuple();
                    Pair::new(f(item), punct)
                })
                .collect(),
        }
    }
}

impl<T: ToTokens> ToTokens for Struct<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            attrs,
            vis,
            r#struct,
            ident,
            brace,
            fields,
        } = self;
        for attr in attrs {
            attr.to_tokens(tokens);
        }
        vis.to_tokens(tokens);
        r#struct.to_tokens(tokens);
        ident.to_tokens(tokens);
        brace.surround(tokens, |tokens| fields.to_tokens(tokens));
    }
}

struct NamedField<M> {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    colon: Token![:],
    r#type: Type,
    make: M,
}

impl<M> NamedField<M> {
    pub fn map<M2>(self, f: impl FnOnce(M) -> M2) -> NamedField<M2> {
        let Self {
            attrs,
            vis,
            ident,
            colon,
            r#type,
            make,
        } = self;
        NamedField {
            attrs,
            vis,
            ident,
            colon,
            r#type,
            make: f(make),
        }
    }
}

impl<M> ToTokens for NamedField<M> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            attrs,
            vis,
            ident,
            colon,
            r#type,
            make: _, // never printed
        } = self;
        for attr in attrs {
            attr.to_tokens(tokens);
        }
        vis.to_tokens(tokens);
        ident.to_tokens(tokens);
        colon.to_tokens(tokens);
        r#type.to_tokens(tokens);
    }
}

impl<T: Parse> Parse for NamedField<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            ident: input.parse()?,
            colon: input.parse()?,
            r#type: input.parse()?,
            make: input.parse()?,
        })
    }
}

struct Make {
    #[expect(dead_code)]
    eq: Token![=],
    expr: Expr,
}

impl Parse for Make {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            eq: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use syn::parse::Parser as _;

    #[test]
    fn empty() {
        t(
            quote! {
                extern struct Red {
                    car: String,
                    apple: usize,
                }
            },
            expect![],
        );
    }

    #[test]
    fn one() {
        t(
            quote! {
                pub struct Red {
                    car: String,
                    pub apple: usize,
                }
            },
            expect![[r#"
                pub struct Red {
                    car: String,
                    pub apple: usize,
                }
            "#]],
        );
    }

    #[test]
    fn smoke() {
        t(
            quote! {
                extern struct Red {
                    car: String,
                    pub apple: usize,
                }

                fn red2amber();

                struct RedAmber {
                    fire: Vec<u8> = Vec::new(),
                }

                /// fn docs
                fn amber2green();

                struct Green {
                    another_apple: u8 = 10,
                }

                struct Amber {
                    /// field docs
                    frozen_insect: (),
                }

                #[allow(unused)]
                fn amber2red(self);

                struct Red2 {
                    pub more_fire: Vec<String> = vec![String::new()],
                }
            },
            expect![[r#"
                fn red2amber(Red { car, apple }: Red) -> RedAmber {
                    RedAmber {
                        car,
                        apple,
                        fire: Vec::new(),
                    }
                }
                struct RedAmber {
                    car: String,
                    pub apple: usize,
                    fire: Vec<u8>,
                }
                /// fn docs
                fn amber2green(RedAmber { car, apple, fire }: RedAmber) -> Green {
                    Green {
                        car,
                        apple,
                        fire,
                        another_apple: 10,
                    }
                }
                struct Green {
                    car: String,
                    pub apple: usize,
                    fire: Vec<u8>,
                    another_apple: u8,
                }
                struct Amber {
                    car: String,
                    pub apple: usize,
                    fire: Vec<u8>,
                    another_apple: u8,
                    /// field docs
                    frozen_insect: (),
                }
                impl Amber {
                    #[allow(unused)]
                    fn amber2red(self) -> Red2 {
                        let Amber { car, apple, fire, another_apple, frozen_insect } = self;
                        Red2 {
                            car,
                            apple,
                            fire,
                            another_apple,
                            frozen_insect,
                            more_fire: vec![String::new()],
                        }
                    }
                }
                struct Red2 {
                    car: String,
                    pub apple: usize,
                    fire: Vec<u8>,
                    another_apple: u8,
                    /// field docs
                    frozen_insect: (),
                    pub more_fire: Vec<String>,
                }
            "#]],
        );
    }

    fn t(input: TokenStream, expect: Expect) {
        let (Ok(toks) | Err(toks)) = _rollup
            .parse2(input)
            .map_err(syn::Error::into_compile_error);
        expect.assert_eq(&prettyplease::unparse(&syn::parse2(toks).unwrap()));
    }
}
