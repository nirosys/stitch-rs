#[derive(Default, Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Let { name: String, expression: Expression},
    Foo,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier { name: String },

    StringLiteral { value: String },
    IntLiteral { value: i32 },
    BooleanLiteral { value: bool },

    Negation { exp: Box<Expression> },
}
