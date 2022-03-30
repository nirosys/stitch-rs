//! This module contains the implementation for parsing stitch programs into
//! an AST.

use std::iter::Peekable;

use crate::ast::{Ast, Statement, Expression};
use crate::lexer::{tokenize, Token, TokenTuple, Location};
use crate::errors::StitchError;

use std::collections::{VecDeque, vec_deque};

pub type ParseResult = Result<Ast, StitchError>;

type TokenIterator<'a> = Peekable<vec_deque::Iter<'a, TokenTuple>>;

// big difference between Fn and fn
type ParseFunction<'a> = fn(&mut TokenIterator<'a>) -> Result<Expression, StitchError>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
    Or,
    And,
    Equal,
    Sum,
    Product,
    Prefix,
    Call,
    Dereference,
}

#[derive(Default)]
struct Parser<'a> {
    prog: &'a str,
    location: Location,

    current: TokenTuple,
    peek: TokenTuple,
}

impl<'a> Parser<'a> {
    /// Create a new parser given
    fn new(prog: &'a str) -> Parser<'a> {
        Parser {
            prog,
            location: Location{ line: 0, column: 0 },
            ..Default::default()
        }
    }

    /// Parse the program provided to `new` into an AST.
    fn parse(&mut self) -> ParseResult {
        let mut tree: Ast = Default::default();
        let tokens = tokenize(self.prog)?;
        let mut tokens = tokens.iter().peekable();

        while let Some((loc, tok)) = tokens.peek() {
            if *tok != Token::Eof {
                println!("looking @ {:?}", tok);
                match self.parse_statement(&mut tokens) {
                    Ok(stmt) => tree.statements.push(stmt),
                    Err(e) => Err(e)?,
                }
            } else {
                tokens.next();
            }
        }
        Ok(tree)
    }

    /// Parse a single statement.
    /// This will parse top-level statements such as 'let', 'import', etc.
    fn parse_statement(&self, tokens: &mut TokenIterator<'a>) -> Result<Statement, StitchError> {
        let (_, tok) = tokens.peek().unwrap(); // This should be safe, since we're only called if peek is not None
        println!("statement: {:?}", tok);
        match tok {
            Token::Let => self.parse_let(tokens),
            _ => panic!("Unknown token at beginning of statement"),
        }
    }

    /// Parse a single Let statement.
    fn parse_let(&self, tokens: &mut TokenIterator<'a>) -> Result<Statement, StitchError> {
        let first: Vec<&TokenTuple> = tokens.take(3).collect();

        match first.as_slice() {
            [(_, Token::Let), (_, Token::Identifier(name)), (_, Token::Assign)] => {
                let let_stmt = Statement::Let {
                    name: name.clone(),
                    expression: self.parse_expression(tokens, Precedence::Lowest)?,
                };

                tokens.next_if(|(_, tok)| &Token::Semicolon == tok);

                Ok(let_stmt)
            }
            _ => panic!("Unexpected token"),
        }
    }

    /// Parse a single expression, taking the current precedence into account.
    fn parse_expression(&self, tokens: &mut TokenIterator<'a>, prec: Precedence) -> Result<Expression, StitchError> {
        println!("Expression: {:?}", tokens.peek());
        let (loc, tok) = tokens.peek().unwrap();

        match prefix_function(tok) {
            None => Err(StitchError::new(loc.line, loc.column, format!("Unexpected token: {:?}.", tok))),
            Some(prefix) => {
                let left = prefix(tokens).unwrap();
                if *tok != Token::Eof && *tok != Token::Semicolon && prec < precedence(tok) {
                    // TODO: infix parsing..
                }
                Ok(left)
            }
        }
    }
}

fn precedence(tok: &Token) -> Precedence {
    use Precedence::*;
    match tok {
        Token::Comma | Token::Assign | Token::Or => Or,
        Token::And => And,
        Token::Plus | Token::Minus => Sum,
        Token::Star | Token::Slash | Token::Modulus => Product,
        Token::LParen => Call,
        // TODO: Add more.
        _ => Lowest,
    }
}


fn infix_function<'a>(tok: &Token) -> Option<ParseFunction<'a>> {
    match tok {
        _ => None,
    }
}

fn prefix_function<'a>(tok: &Token) -> Option<ParseFunction<'a>> {
    // TODO: these simple closures could be a macro.
    match tok {
        Token::Str(_) => Some(|tokens| -> Result<Expression, StitchError> {
            if let Some((_, Token::Str(s))) = tokens.next() {
                Ok(Expression::StringLiteral{value: s.clone()})
            } else {
                Err(StitchError::new(0, 0, "Expected string literal".to_owned()))
            }
        }),
        Token::Integer(_) => Some(|tokens| -> Result<Expression, StitchError> {
            if let Some((_, Token::Integer(i))) = tokens.next() {
                Ok(Expression::IntLiteral{value: *i})
            } else {
                Err(StitchError::new(0, 0, "Expected string literal".to_owned()))
            }
        }),
        Token::Identifier(_) => Some(|tokens| -> Result<Expression, StitchError> {
            if let Some((_, Token::Identifier(name))) = tokens.next() {
                Ok(Expression::Identifier{name: name.clone()})
            } else {
                Err(StitchError::new(0, 0, "Expected identifier".to_owned()))
            }
        }),
        Token::True | Token::False => Some(|tokens| -> Result<Expression, StitchError> {
            if let Some((_, tok)) = tokens.next() {
                Ok(Expression::BooleanLiteral{value: tok == &Token::True})
            } else {
                Err(StitchError::new(0, 0, "Expected boolean literal".to_owned()))
            }
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// This test just validates that we are parsing a single statement.
    #[test]
    fn test_single_statement() {
        struct Test {
            prog: &'static str,
            statements: usize,
        }
        let tests: &[Test] = &[
            Test{prog: r#"let foo = "bar""#, statements: 1},
        ];

        for test in tests {
            let mut parser = Parser::new(test.prog);
            let ast = parser.parse().expect("Error parsing statement.");
            println!("AST: {:?}", ast);

            assert_eq!(ast.statements.len(), test.statements);
        }
    }

    #[test]
    fn test_let_statement() {
        struct Test {
            prog: &'static str,
            name: &'static str,
            exp: Expression,
        }
        let tests: &[Test] = &[
            Test{prog: r#"let foo = "bar""#, name: "foo", exp: Expression::StringLiteral{value: "bar".to_owned()} },
            Test{prog: r#"let foo = 1;"#,    name: "foo", exp: Expression::IntLiteral{value: 1} },
            Test{prog: r#"let foo = bar;"#,  name: "foo", exp: Expression::Identifier{name: "bar".to_owned()} },
            Test{prog: r#"let foo = true;"#, name: "foo", exp: Expression::BooleanLiteral{value: true} },
            Test{prog: r#"let foo = false;"#, name: "foo", exp: Expression::BooleanLiteral{value: false} },
        ];

        for test in tests {
            let mut parser = Parser::new(test.prog);
            let ast = parser.parse().expect("Error parsing statement.");

            assert_eq!(ast.statements.len(), 1);
            if let Statement::Let{name, expression} = &ast.statements[0] {
                assert_eq!(name, test.name);
                assert_eq!(expression, &test.exp);
            } else {
                assert!(false);
            }
        }
    }
}
