use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{parse::*, punctuated::Punctuated, *};
mod rollup;

/// Generate multiple structs which accumulate fields,
/// and optionally functions which convert between them.
///
/// ```
/// strux::rollup! {
///     struct Red {
///         apple: usize,
///     }
///
///     struct RedYellow {
///         banana: bool,
///         lemon: f32,
///     }
///
///     // generate a conversion function RedYellow -> RedYellowGreen
///     fn add_green();
///
///                                   // you can add attributes and
///     #[derive(Default, PartialEq)] // visibility every you'd expect
///     pub struct RedYellowGreen {
///         lime: String = String::from("I'm a lime!"),
///                   // ^ you must specify how the conversion
///                   //   fills in this field.
///
///         /// some docs
///         pub avocado: Vec<u8> = vec![],
///     }
/// }
///
///
/// let red = Red { apple: 0 };
/// let red_yellow = RedYellow {
///     apple: 0,     // contains fields from the preceding struct
///     banana: true, // AND the ones specified
///     lemon: 0.0,
/// };
///
/// let red_yellow_green = add_green(red_yellow);
///
/// assert!(red_yellow_green == RedYellowGreen {
///     banana: true,
///     lime: String::from("I'm a lime!"),
///     ..Default::default()
/// } );
/// ```
///
/// You may also start the chain with a struct defined elsewhere:
///
/// ```
/// mod elsewhere {
///     #[derive(Default)]
///     pub struct Red {
///         pub apple: usize,
///         pub cherry: u8,
///     }
/// }
///
/// use elsewhere::Red;
///
/// strux::rollup! {
///     extern struct Red {
///         apple: usize,
///         cherry: u8,
///     }
///
///     fn add_yellow();
///
///     #[derive(Default, PartialEq)]
///     struct RedYellow {
///         banana: bool = true,
///     }
/// }
///
/// let red = Red::default();
/// let red_yellow = add_yellow(red);
/// assert!(red_yellow == RedYellow {
///     banana: true,
///     ..Default::default()
/// });
/// ```
#[proc_macro]
pub fn rollup(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    parse_macro_input!(input with rollup::_rollup).into()
}

#[proc_macro]
pub fn strux(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse_macro_input!(item with _strux).into()
}

fn _strux(input: ParseStream) -> syn::Result<TokenStream> {
    let (fwds, hdr, rows) = parse(input)?;
    for row in &rows {
        if row.cells.len() != hdr.cells.len() {
            let mut e = syn::Error::new(
                row.ident.span(),
                format!("{} columns in row", row.cells.len()),
            );
            e.combine(syn::Error::new(
                hdr.struct_token.span,
                format!("{} columns in header", hdr.cells.len()),
            ));
            return Err(e);
        }
    }
    let mut defns = vec![];
    for FwdDecl {
        attrs,
        vis,
        ident,
        fields,
    } in fwds
    {
        defns.push(Defn {
            attrs,
            vis,
            ident,
            fields: match fields {
                FwdFields::None(_) => vec![],
                FwdFields::Some { brace: _, fields } => fields
                    .into_iter()
                    .map(|field| {
                        let FwdField {
                            attrs,
                            vis,
                            ident,
                            kind,
                        } = field;
                        Ok(DefnField {
                            attrs,
                            vis,
                            ty: match kind {
                                FwdFieldKind::Unique { colon: _, ty } => ty,
                                FwdFieldKind::Shared => rows
                                    .iter()
                                    .find_map(|row| (row.ident == ident).then(|| row.ty.clone()))
                                    .ok_or_else(|| {
                                        syn::Error::new(ident.span(), "no type for shared field")
                                    })?,
                            },
                            ident,
                        })
                    })
                    .collect::<Result<_>>()?,
            },
        });
    }
    let Header {
        attrs,
        vis,
        struct_token: _,
        begin: _,
        cells: headings,
    } = &hdr;
    for defn in &mut defns {
        if matches!(defn.vis, Visibility::Inherited) {
            defn.vis.clone_from(vis);
        }
        prepend(&mut defn.attrs, attrs);
    }
    for (ident, _) in headings {
        if !defns.iter().any(|it| &it.ident == ident) {
            defns.push(Defn {
                attrs: attrs.clone(),
                vis: vis.clone(),
                ident: ident.clone(),
                fields: vec![],
            });
        }
    }

    for Row {
        attrs,
        padding: _,
        vis,
        ident,
        colon: _,
        ty,
        begin: _,
        cells,
    } in rows
    {
        for defn in &mut defns {
            let Some(ix) = headings
                .iter()
                .enumerate()
                .find_map(|(ix, (heading, _))| (heading == &defn.ident).then_some(ix))
            else {
                continue;
            };
            let is_selected = matches!(cells[ix], (Some(_), _));
            if !is_selected {
                continue;
            }

            match defn.fields.iter_mut().find(|it| it.ident == ident) {
                Some(field) => {
                    prepend(&mut field.attrs, &attrs);
                }
                None => defn.fields.push(DefnField {
                    attrs: attrs.clone(),
                    vis: vis.clone(),
                    ident: ident.clone(),
                    ty: ty.clone(),
                }),
            }
        }
    }

    Ok(quote!(#(#defns)*))
}

#[cfg_attr(test, derive(Debug))]
struct Defn {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    fields: Vec<DefnField>,
}

impl ToTokens for Defn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            attrs,
            vis,
            ident,
            fields,
        } = self;
        tokens.extend(quote! {
            #(#attrs)*
            #vis struct #ident {
                #(#fields,)*
            }
        });
    }
}

#[cfg_attr(test, derive(Debug))]
struct DefnField {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    ty: Type,
}

impl ToTokens for DefnField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            attrs,
            vis,
            ident,
            ty,
        } = self;
        tokens.extend(quote! {
            #(#attrs)*
            #vis #ident: #ty
        });
    }
}

fn parse(input: ParseStream<'_>) -> syn::Result<(Vec<FwdDecl>, Header, Vec<Row>)> {
    let mut fwd = vec![];
    let hdr = loop {
        let attrs = attrs(input)?;
        let vis = input.parse()?;
        let lo = input.lookahead1();
        if lo.peek(Ident) {
            let ident = input.parse::<Ident>()?;
            fwd.push(FwdDecl {
                attrs,
                vis,
                ident,
                fields: input.parse()?,
            });
        } else if lo.peek(Token![struct]) {
            break Header {
                attrs,
                vis,
                struct_token: input.parse::<Token![struct]>()?,
                begin: input.parse()?,
                cells: {
                    let mut cols = vec![];
                    while input.peek(Ident) && input.peek2(Token![|]) {
                        cols.push((input.parse::<Ident>()?, input.parse::<Token![|]>()?));
                    }
                    cols
                },
            };
        } else {
            return Err(lo.error());
        }
    };
    let mut rows = vec![];
    while !input.is_empty() {
        rows.push(Row {
            attrs: attrs(input)?,
            vis: input.parse()?,
            padding: {
                let mut padding = vec![];
                while input.peek(Token![.]) {
                    padding.push(input.parse::<Token![.]>()?);
                }
                padding
            },
            ident: input.parse()?,
            colon: input.parse()?,
            ty: input.parse()?,
            begin: input.parse()?,
            cells: {
                let mut cols = vec![];
                loop {
                    match input.parse()? {
                        x @ Some(_) => cols.push((x, input.parse::<Token![|]>()?)),
                        x @ None if input.peek(Token![|]) => cols.push((x, input.parse()?)),
                        _ => break,
                    }
                }
                cols
            },
        });
    }
    Ok((fwd, hdr, rows))
}

fn prepend<T: Clone>(dst: &mut Vec<T>, src: &[T]) {
    dst.splice(0..0, src.iter().cloned());
}

fn attrs(input: ParseStream<'_>) -> syn::Result<Vec<Attribute>> {
    Ok(match input.peek(Token![#]) {
        true => Attribute::parse_outer(input)?,
        false => vec![],
    })
}

#[cfg_attr(test, derive(Debug))]
struct FwdDecl {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    fields: FwdFields,
}

#[cfg_attr(test, derive(Debug))]
enum FwdFields {
    #[expect(unused)]
    None(Token![;]),
    Some {
        #[expect(unused)]
        brace: token::Brace,
        fields: Punctuated<FwdField, Token![,]>,
    },
}

impl Parse for FwdFields {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lo = input.lookahead1();
        if lo.peek(Token![;]) {
            Ok(Self::None(input.parse::<Token![;]>()?))
        } else if lo.peek(token::Brace) {
            let content;
            Ok(Self::Some {
                brace: braced!(content in input),
                fields: Punctuated::parse_terminated(&content)?,
            })
        } else {
            Err(lo.error())
        }
    }
}

#[cfg_attr(test, derive(Debug))]
struct FwdField {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    kind: FwdFieldKind,
}

#[cfg_attr(test, derive(Debug))]
#[allow(clippy::large_enum_variant)]
enum FwdFieldKind {
    Shared,
    Unique {
        #[expect(unused)]
        colon: Token![:],
        ty: Type,
    },
}

impl Parse for FwdField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: attrs(input)?,
            vis: input.parse()?,
            ident: input.parse()?,
            kind: match input.parse()? {
                Some(colon) => FwdFieldKind::Unique {
                    colon,
                    ty: input.parse()?,
                },
                None => FwdFieldKind::Shared,
            },
        })
    }
}

#[cfg_attr(test, derive(Debug))]
struct Header {
    attrs: Vec<Attribute>,
    vis: Visibility,
    struct_token: Token![struct],
    #[expect(unused)]
    begin: Token![|],
    cells: Vec<(Ident, Token![|])>,
}

#[cfg_attr(test, derive(Debug))]
struct Row {
    attrs: Vec<Attribute>,
    #[expect(unused)]
    padding: Vec<Token![.]>,
    vis: Visibility,
    ident: Ident,
    #[expect(unused)]
    colon: Token![:],
    ty: Type,
    #[expect(unused)]
    begin: Token![|],
    cells: Vec<(Option<kw::X>, Token![|])>,
}

mod kw {
    syn::custom_keyword!(X);
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn smoke() {
        let tokens = _strux
            .parse2(quote! {
                    /// Baz comment.
                    Baz {
                        /// Baz.s comment
                        s: &'static str,
                    }


                    /// Shared comment
                    struct     | Foo | Bar | Baz |
                    /// u comment
                    u: usize   |  X  |  X  |     |
                    s: String  |  X  |     |  X  |
                    v: Vec<u8> |     |  X  |     |
            })
            .unwrap();
        let s = prettyplease::unparse(&syn::parse2(tokens).unwrap());
        expect![[r#"
            /// Shared comment
            /// Baz comment.
            struct Baz {
                /// Baz.s comment
                s: &'static str,
            }
            /// Shared comment
            struct Foo {
                /// u comment
                u: usize,
                s: String,
            }
            /// Shared comment
            struct Bar {
                /// u comment
                u: usize,
                v: Vec<u8>,
            }
        "#]]
        .assert_eq(&s);
    }
}
