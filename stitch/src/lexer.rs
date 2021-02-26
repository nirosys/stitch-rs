//! Module for tokenizing stitch programs.
//!

use super::errors::StitchError;

use std::iter::Peekable;
use std::str::CharIndices;
use std::collections::VecDeque;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Let,
    Import,
    Node,
    Function,
    Modifier,
    Internal,
    If,
    Else,
    True,
    False,
    And,
    Or,
    Foreach,
    In,
    Integer(i32),
    Float(f32),
    Str(String),
    Identifier(String),

    Comment(String),
    Eof,
}

pub type TokenTuple = (usize, Token);

struct Lexer<'a> {
   iter: Peekable<CharIndices<'a>>,
   prog: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(prog: &'a str) -> Lexer<'a> {
        Lexer {
            prog: prog,
            iter: prog.char_indices().peekable(),
        }
    }

    fn tokenize(&mut self) -> Result<VecDeque<TokenTuple>, StitchError> {
        let mut tokens = VecDeque::new();
        loop {
            match self.iter.next() {
                Some((pos, ch)) => {
                    match ch {
                        'a' ..= 'z' | 'A' ..= 'Z' | '_' => {
                            let ident = self.consume_identifier(ch);
                            tokens.push_back((pos, ident));
                        }
                        '0' ..= '9' =>
                            tokens.push_back((pos, self.consume_number(ch, false))),
                        '"' =>
                            tokens.push_back((pos, self.consume_wrapped('"', |b| Ok(Token::Str(b)))?)),
                        ' ' | '\n' | '\t' | '\r' => {},
                        c => {
                            return Err(StitchError::new(0, 0, format!("unexpected character: {}", c)));
                        },
                    }
                }
                None => {
                    tokens.push_back((self.prog.len(), Token::Eof));
                    return Ok(tokens)
                }
            }
        }
    }

    fn consume_while<F>(&mut self, mut buffer: String, predicate: F) -> String
        where F: Fn(char) -> bool
    {
        loop {
            match self.iter.peek() {
                None => break,
                Some(&(_, c)) if !predicate(c) => break,
                Some(&(_, c)) => {
                    buffer.push(c);
                    self.iter.next();
                }
            }
        }
        buffer
    }

    fn consume_wrapped<F>(&mut self, wrapper: char, handler: F) -> Result<Token, StitchError>
        where F: Fn(String) -> Result<Token, String>
    {
        let mut buffer = String::new();
        while let Some((_, ch)) = self.iter.next() {
            if ch == wrapper {
                return handler(buffer).map_err(|e| StitchError::new(0, 0, e));
            } else if ch == '\\' {
                buffer.push(ch);
                if let Some((_, ch)) = self.iter.next() {
                    buffer.push(ch);
                }
            } else {
                buffer.push(ch);
            }
        }

        let msg = format!("expected closing '{}'", wrapper);
        Err(StitchError::new(0, 0, msg))
    }


    fn consume_number(&mut self, first: char, is_negative: bool) -> Token {
        let i = self.consume_while(first.to_string(), |c| c.is_digit(10));
        let numeric: i32 = i.parse().expect("expected numeric value");
        if is_negative {
            Token::Integer(numeric * -1)
        } else {
            Token::Integer(numeric)
        }
    }

    fn consume_identifier(&mut self, first: char) -> Token {
        let ident = self.consume_while(first.to_string(), |c| c.is_alphanumeric() || c == '_');
        match ident.as_ref() {
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            "and" => Token::And,
            "or" => Token::Or,
            "foreach" => Token::Foreach,
            "in" => Token::In,
            "internal" => Token::Internal,
            "import" => Token::Import,
            "node" => Token::Node,
            "fn" => Token::Function,
            "mod" => Token::Modifier,
            _ => Token::Identifier(ident),
        }
    }
}

pub fn tokenize(prog: &str) -> Result<VecDeque<TokenTuple>, StitchError> {
    println!("TOKENIZING: {}", prog);
    Lexer::new(prog).tokenize()
}

#[cfg(test)]
mod tests {

   use super::*;
   use super::Token::*;

   macro_rules! test_keywords {
       ( $( $x:expr, $y:ident),* ) => {
           $(
               assert_eq!(tokenize_queue($x), vec![(0, $y), ($x.len(), Eof)]);
           )*
       };
   }

   fn tokenize_queue(prog: &str) -> Vec<TokenTuple> {
      let mut result = tokenize(prog).unwrap();
      let mut v = Vec::new();
      while let Some(tok) = result.pop_front() {
         v.push(tok);
      }
      v
   }

   #[test]
   fn tokenize_basic_test() {
       assert_eq!(tokenize_queue("10"), vec![(0, Integer(10)), (2, Eof)]);
       assert_eq!(tokenize_queue("foo"), vec![(0, Identifier(String::from("foo"))), (3, Eof)]);
       assert_eq!(tokenize_queue("\"foo\""), vec![(0, Str(String::from("foo"))), (5, Eof)]);

       test_keywords!(
           "let", Let,
           "if", If,
           "else", Else,
           "true", True,
           "false", False,
           "and", And,
           "or", Or,
           "foreach", Foreach,
           "in", In,
           "internal", Internal,
           "import", Import,
           "node", Node,
           "fn", Function,
           "mod", Modifier
       );
   }

   #[test]
   fn tokenize_basic_errors() {
       assert!(tokenize("\"foo")
               .unwrap_err()
               .to_string()
               .contains("expected closing '\"'"));
   }
}
