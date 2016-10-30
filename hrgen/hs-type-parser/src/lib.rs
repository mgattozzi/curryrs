extern crate combine;
extern crate combine_language;
use combine::Parser;
use std::borrow::Cow;
use std::marker::PhantomData;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
	Constr(Cow<'static, str>),
	Var(String),
	App(Box<Type>, Box<Type>),
}

pub fn constr<S: Into<Cow<'static, str>>>(name: S) -> Type {
	Type::Constr(name.into())
}

pub fn var<S: Into<String>>(name: S) -> Type {
	Type::Var(name.into())
}

pub fn app(l: Type, r: Type) -> Type {
	Type::App(Box::new(l), Box::new(r))
}

pub fn app2(x: Type, y: Type, z: Type) -> Type {
	app(app(x, y), z)
}

pub fn func(l: Type, r: Type) -> Type {
	app2(constr("->"), l, r)
}

#[derive(Clone, Copy)]
struct TypeParser<I>(PhantomData<I>);

impl<I> Parser for TypeParser<I>
	where I: combine::Stream<Item = char>
{
	type Input = I;
	type Output = Type;
	fn parse_lazy(&mut self, input: I) -> combine::ConsumedResult<Type, I> {
		use combine::*;
		use combine::char::*;
		use combine_language::*;

		fn char_join(cs: (char, String)) -> String {
			let (c, mut s) = cs;
			s.insert(0, c);
			s
		}

		fn is_ident_tail(c: char) -> bool {
			c.is_alphanumeric() || c == '\'' || c == '#'
		}

		// Terminals
		let var_parser = try(lex((lower(), many(satisfy(is_ident_tail))))
			.map(char_join).map(var));
		let constr_parser = try(lex((upper(), many(satisfy(is_ident_tail))))
			.map(char_join).map(constr));
		let arrow = try(lex(string("->"))).map(|s| constr(s));
		let unit = try(lex(string("()"))).map(constr);

		// The recursive parenthesized expression parser
		let paren = try(lex((char('('), type_parser(), char(')')))).map(|t| t.1);

		// Any term
		let term = unit.or(paren).or(constr_parser).or(var_parser);

		// We have two infix operators: the function arrow, and application as
		// juxtaposition. We need to try to parse the arrow first, as it may be
		// preceeded by whitespace.
		let op_parser = arrow.map(Some).or(skip_many1(space()).map(|_| None)).map(|op| {
			match op {
				Some(_) => (op, Assoc { precedence: 1, fixity: Fixity::Right }),
				None => (op, Assoc { precedence: 2, fixity: Fixity::Left }),
			}
		});
		let mut expr = expression_parser(term, op_parser, |l, o, r| {
			match o {
				Some(op) => app2(op, l, r),
				None => app(l, r),
			}
		});
		expr.parse_lazy(input)
	}
}

fn type_parser<I: combine::Stream<Item = char>>() -> TypeParser<I> {
	TypeParser(PhantomData)
}

/// Parse a Haskell type
pub fn parse_type(input: &str) -> Result<(Type, &str), combine::ParseError<&str>> {
	type_parser().parse(input)
}

// Parsing support

#[derive(Clone, Copy)]
struct Whitespace<I>(PhantomData<I>);

impl<I> Parser for Whitespace<I>
	where I: combine::Stream<Item = char>
{
	type Input = I;
	type Output = ();
	fn parse_lazy(&mut self, input: I) -> combine::ConsumedResult<(), I> {
		combine::char::spaces().map(|_| ()).parse_lazy(input)
	}
}

fn whitespace<I: combine::Stream<Item = char>>() -> Whitespace<I> {
	Whitespace(PhantomData)
}

enum Right {}

trait Selector<Input> {
	type Output;
	fn select(t: Input) -> Self::Output;
}

impl<L, R> Selector<(L, R)> for Right {
	type Output = R;
	fn select(t: (L, R)) -> Self::Output { t.1 }
}

/// Sequence two parsers. Keep the selected result and discard the other.
#[derive(Clone, Copy)]
struct Seq<S, L, R>(L, R, PhantomData<S>);

impl<S, L, R> Parser for Seq<S, L, R>
	where L: Parser<Input=R::Input>,
			R: Parser,
			S: Selector<(L::Output, R::Output)>
{
	type Input = R::Input;
	type Output = S::Output;
	fn parse_lazy(&mut self, input: Self::Input) -> combine::ConsumedResult<Self::Output, Self::Input> {
		(&mut self.0, &mut self.1).parse_lazy(input).map(|t| S::select(t))
	}
}

fn seqr<L, R>(l: L, r: R) -> Seq<Right, L, R>
	where L: Parser<Input=R::Input>, R: Parser
{
	Seq(l, r, PhantomData::<Right>)
}

fn lex<P: Parser<Input=I>, I>(p: P) -> Seq<Right, Whitespace<I>, P>
	where I: combine::Stream<Item = char>
{
	seqr(whitespace(), p)
}

#[test]
fn parse_type_test() {
	assert_eq!(parse_type("Int"), Ok(((constr("Int")), "")));
	assert_eq!(parse_type("a"), Ok(((var("a")), "")));
	assert_eq!(parse_type("(a)"), Ok(((var("a")), "")));
	assert_eq!(parse_type("Vec a"), Ok((app(constr("Vec"), var("a")), "")));
	assert_eq!(parse_type("f   a"), Ok((app(var("f"), var("a")), "")));
	assert_eq!(parse_type("In a -> Out"),
	         Ok((func(app(constr("In"), var("a")), constr("Out")), "")));
	assert_eq!(parse_type("In a -> (In b -> Out) -> Out"),
	         Ok((func(app(constr("In"), var("a")),
	               func(func(app(constr("In"), var("b")), constr("Out")), constr("Out"))),
	            "")));
	assert_eq!(parse_type("FunPtr (I32 -> IO I32) -> IO ()"),
	           Ok((func(app(constr("FunPtr"),
	                        func(constr("I32"), app(constr("IO"), constr("I32")))),
	                    app(constr("IO"), constr("()"))),
	               "")));
}
