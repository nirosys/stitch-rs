//! Module for tokenizing stitch programs.
//!

use super::errors::StitchError;

use std::iter::Peekable;
use std::str::CharIndices;
use std::collections::VecDeque;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    // Keywords
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

    // Operators
    Minus,
    Plus,
    Slash,
    Star,
    Modulus,
    Assign,
    Equal,
    Less,
    Great,
    LessEq,
    GreatEq,
    Not,
    NotEq,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semicolon,

    // Literals
    Integer(i32),
    Float(f32),
    Str(String),

    Identifier(String),

    Comment(String),
    Eof,
}

impl Default for Token {
    fn default() -> Self {
        Token::Eof
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

pub type TokenTuple = (Location, Token);


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
        let mut line = 0;
        let mut last_pos = 0;
        let mut column = 0;
        loop {
            match self.iter.next() {
                Some((pos, ch)) => {
                    column = column + (pos - last_pos);
                    last_pos = pos;
                    //println!("Column: {}", column);
                    let loc = Location{line: line, column: column};
                    match ch {
                        'a' ..= 'z' | 'A' ..= 'Z' | '_' => {
                            let ident = self.consume_identifier(ch);
                            tokens.push_back((loc, ident));
                        }
                        '0' ..= '9' =>
                            tokens.push_back((loc, self.consume_number(ch, false))),
                        '"' =>
                            tokens.push_back((loc, self.consume_wrapped('"', |b| Ok(Token::Str(b)))?)),
                        ' ' | '\t' => {}
                        '\n' | '\r' => {
                            line = line + 1;
                            column = 0;
                            last_pos += 1; // This accounts for the invisible newline, and later calculations for column.
                        },
                        '#' => {
                            //let comment = self.consume_comment();
                            //tokens.push_back((loc, comment));
                        },
                        '-' => {
                            if let Some(&(_, c)) = self.iter.peek() {
                                if c.is_numeric() {
                                    tokens.push_back((loc, self.consume_number('0', true)));
                                } else {
                                    tokens.push_back((loc, Token::Minus));
                                }
                            }
                        },
                        '+' => tokens.push_back((loc, Token::Plus)),
                        '*' => tokens.push_back((loc, Token::Star)),
                        '/' => tokens.push_back((loc, Token::Slash)),
                        '%' => tokens.push_back((loc, Token::Modulus)),
                        '(' => tokens.push_back((loc, Token::LParen)),
                        ')' => tokens.push_back((loc, Token::RParen)),
                        '{' => tokens.push_back((loc, Token::LBrace)),
                        '}' => tokens.push_back((loc, Token::RBrace)),
                        '[' => tokens.push_back((loc, Token::LBracket)),
                        ']' => tokens.push_back((loc, Token::RBracket)),
                        ';' => tokens.push_back((loc, Token::Semicolon)),
                        ',' => tokens.push_back((loc, Token::Comma)),
                        '=' => {
                            let mut tok = Token::Assign;
                            if let Some(&(_, c)) = self.iter.peek() {
                                if c == '=' {
                                    self.iter.next();
                                    tok = Token::Equal;
                                }
                            }
                            tokens.push_back((loc, tok));
                        },
                        '<' => {
                            let mut tok = Token::Less;
                            if let Some(&(_, c)) = self.iter.peek() {
                                if c == '=' {
                                    self.iter.next();
                                    tok = Token::LessEq;
                                }
                            }
                            tokens.push_back((loc, tok));
                        },
                        '>' => {
                            let mut tok = Token::Great;
                            if let Some(&(_, c)) = self.iter.peek() {
                                if c == '=' {
                                    self.iter.next();
                                    tok = Token::GreatEq;
                                }
                            }
                            tokens.push_back((loc, tok));
                        },
                        '!' => {
                            let mut tok = Token::Not;
                            if let Some(&(_, c)) = self.iter.peek() {
                                if c == '=' {
                                    self.iter.next();
                                    tok = Token::NotEq;
                                }
                            }
                            tokens.push_back((loc, tok));
                        },
                        c => {
                            let msg = format!("unexpected character: '{}'", c);
                            return Err(StitchError::new(line, 0, msg));
                        },
                    }
                }
                None => {
                    let column = column + self.prog.len() - last_pos;
                    let loc = Location{line: line, column: column};
                    tokens.push_back((loc, Token::Eof));
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
            } else if ch == '\n' { // Tokens shouldn't span multiple lines, not even strings.
                break;
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
    Lexer::new(prog).tokenize()
}

#[cfg(test)]
mod tests {

   use super::*;
   use super::Token::*;

   macro_rules! test_keywords {
       ( $( ($x:expr, $y:ident) ),* ) => {
           $( assert_eq!(tokenize_queue($x), vec![(loc!(0, 0), $y), (loc!(0, $x.len()), Eof)]); )*
       };
   }

   macro_rules! loc {
       ($line:expr, $col:expr) => {
           {//println!("Location: {}, {}", $line, $col);
           Location{line: $line, column: $col}}
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
   fn correct_locations() {
       assert_eq!(tokenize_queue("1 2"), vec![(loc!(0, 0), Integer(1)), (loc!(0, 2), Integer(2)), (loc!(0, 3), Eof)]);
       assert_eq!(tokenize_queue("1\n2"), vec![(loc!(0, 0), Integer(1)), (loc!(1, 0), Integer(2)), (loc!(1, 1), Eof)]);
   }

   #[test]
   fn tokenize_basic_test() {
       assert_eq!(tokenize_queue("10"), vec![(loc!(0, 0), Integer(10)), (loc!(0, 2), Eof)]);
       assert_eq!(tokenize_queue("foo"), vec![(loc!(0, 0), Identifier(String::from("foo"))), (loc!(0, 3), Eof)]);
   }

   #[test]
   fn tokenize_operators() {
       assert_eq!(tokenize_queue("="), vec![(loc!(0, 0), Assign), (loc!(0, 1), Eof)]);
       assert_eq!(tokenize_queue("=="), vec![(loc!(0, 0), Equal), (loc!(0, 2), Eof)]);

       assert_eq!(tokenize_queue("<"), vec![(loc!(0, 0), Less), (loc!(0, 1), Eof)]);
       assert_eq!(tokenize_queue("<="), vec![(loc!(0, 0), LessEq), (loc!(0, 2), Eof)]);

       assert_eq!(tokenize_queue(">"), vec![(loc!(0, 0), Great), (loc!(0, 1), Eof)]);
       assert_eq!(tokenize_queue(">="), vec![(loc!(0, 0), GreatEq), (loc!(0, 2), Eof)]);

       assert_eq!(tokenize_queue("!"), vec![(loc!(0, 0), Not), (loc!(0, 1), Eof)]);
       assert_eq!(tokenize_queue("!="), vec![(loc!(0, 0), NotEq), (loc!(0, 2), Eof)]);
   }

   #[test]
   fn tokenize_keywords() {
       test_keywords!(
           ("let",      Let),
           ("if",       If),
           ("else",     Else),
           ("true",     True),
           ("false",    False),
           ("and",      And),
           ("or",       Or),
           ("foreach",  Foreach),
           ("in",       In),
           ("internal", Internal),
           ("import",   Import),
           ("node",     Node),
           ("fn",       Function),
           ("mod",      Modifier)
       );
   }

   #[test]
   fn tokenize_integers() {
       assert_eq!(tokenize_queue("12345"), vec![(loc!(0, 0), Integer(12345)), (loc!(0, 5), Eof)]);
       assert_eq!(tokenize_queue("-152"), vec![(loc!(0, 0), Integer(-152)), (loc!(0, 4), Eof)]);
       assert_eq!(tokenize_queue(" 12"), vec![(loc!(0, 1), Integer(12)), (loc!(0, 3), Eof)]);
       assert_eq!(tokenize_queue("\n10"), vec![(loc!(1, 0), Integer(10)), (loc!(1, 2), Eof)]);
   }

   #[test]
   fn tokenize_strings() {
       assert_eq!(tokenize_queue("\"foo\""), vec![(loc!(0, 0), Str(String::from("foo"))), (loc!(0, 5), Eof)]);
   }


   #[test]
   fn tokenize_basic_errors() {
       assert!(tokenize("\"foo")
               .unwrap_err()
               .to_string()
               .contains("expected closing '\"'"));
       // No newlines mid-string.
       assert!(tokenize("\"foo\n\"")
               .unwrap_err()
               .to_string()
               .contains("expected closing '\"'"));
   }
}
