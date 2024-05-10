use core::panic;

use crate::lexer::{Operator, Token};

#[derive(Debug, Clone)]
pub enum Stmt {
    None,
    Return(Expr),
    Let(String, Expr),
    VarAssign(String, Expr),
    Scope(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<IfPredicate>),
    Else(Box<Stmt>)
}

#[derive(Debug, Clone)]
pub struct IfPredicate {
    pub cond : Expr,
    pub body : Box<Stmt>,
    pub predicate : Box<Option<IfPredicate>>
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i32),
    VarRef(String),
    BinaryOp(Box<Expr>, Box<Expr>, Operator),
    UnaryOp(Box<Expr>, Operator),
    Scope(Vec<Stmt>)
}

struct Parser {
    current : Token,
    index : usize,
    tokens : Vec<Token>
}

impl Parser {
    fn next(&mut self) -> Option<Token> {
        let t = self.tokens.get(self.index).cloned();
        self.current = t.clone().unwrap_or(Token::None);
        self.index += 1;
        t
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.index).cloned()
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.current.clone() {
            Token::Ret => {
                self.next();
                let expr = self.parse_expr();
                if let Some(Token::SemiColon) = self.next() {
                    return Stmt::Return(expr);
                } else {
                    panic!("Expected semicolon.")
                }
            },
            Token::Let => {
                if let Some(Token::Identifier(ident)) = self.next() {
                    if let Some(Token::Equals) = self.next() {
                        self.next();
                        let expr = self.parse_expr();
                        if let Some(Token::SemiColon) = self.next() {
                            return Stmt::Let(ident, expr);
                        } else {
                            panic!("Expected ';'.")
                        }
                    } else {
                        panic!("Expected '='.");
                    }
                } else {
                    panic!("Expected identifier.");
                }
            },
            Token::Identifier(ident) => {
                if let Some(Token::Equals) = self.next() {
                    self.next();
                    let expr = self.parse_expr();
                    if let Some(Token::SemiColon) = self.next() {
                        return Stmt::VarAssign(ident, expr);
                    } else {
                        panic!("Expected semicolon.")
                    }
                } else {
                    panic!("Expected '='.");
                }
            },
            Token::If => {
                self.next();
                let cond = self.parse_expr();
                self.next();
                let stmt = self.parse_stmt();
                return Stmt::If(cond, Box::new(stmt), self.parse_if_predicate());
            },
            Token::Else => {
                self.next();
                let stmt = self.parse_stmt();
                return Stmt::Else(Box::new(stmt));
            },
            Token::OpenBrace => {
                let mut stmts = Vec::new();
                loop {
                    match self.peek().clone() {
                        Some(Token::CloseBrace) => {
                            self.next();
                            break;
                        },
                        None => break,
                        _ => {
                            self.next();
                            stmts.push(self.parse_stmt());
                        }
                    }
                }
                return Stmt::Scope(stmts);
            }
            t => panic!("Token {:?} could not be parsed.", t)
        }
    }

    fn parse_if_predicate(&mut self) -> Option<IfPredicate> {
        if self.peek() != Some(Token::Elif) {
            return None;
        }
        self.next();
        self.next();
        let cond = self.parse_expr();
        self.next();
        let body = Box::new(self.parse_stmt());
        Some(IfPredicate{cond, body, predicate: Box::new(self.parse_if_predicate())})
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_binop(0)
    }

    fn parse_binop(&mut self, min_prec : i32) -> Expr {
        let mut lhs = self.parse_term();
        loop {
            let (prec, op) = match self.peek().clone() {
                Some(Token::Operator(ref op)) => {
                    (Self::get_precidence(op.clone()), op.clone())
                },
                _ => break
            };
            if prec < min_prec {
                break;
            }

            self.next();
            self.next();
            let rhs = self.parse_binop(prec + 1);
            lhs = Expr::BinaryOp(Box::new(lhs), Box::new(rhs), op);
        }
        lhs
    }

    fn get_precidence(op : Operator) -> i32 {
        match op {
            Operator::Equal | Operator::Less | Operator::LessEqual |
                Operator::Greater | Operator::GreaterEqual => 0,
            Operator::Add | Operator::Sub => 1,
            Operator::And | Operator::Or | Operator::XOr => 2,
            Operator::LShift | Operator::RShift => 3,
            Operator::Mul | Operator::Div => 4,
            Operator::Not => 5
        }
    }

    fn parse_term(&mut self) -> Expr {
        match self.current.clone() {
            Token::Integer(num) => Expr::Integer(num),
            Token::Identifier(ident) => Expr::VarRef(ident),
            Token::OpenParen => {
                self.next();
                let term = self.parse_expr();
                match self.next() {
                    Some(Token::CloseParen) => return term,
                    _ => panic!("Expected ')'.")
                }
            },
            Token::OpenBrace => {
                self.next();
                let mut stmts = Vec::new();
                loop {
                    match self.peek().clone() {
                        Some(Token::CloseBrace) => {
                            self.next();
                            break;
                        },
                        _ => {
                            stmts.push(self.parse_stmt());
                        }
                    }
                }
                return Expr::Scope(stmts);
            },
            Token::Operator(op) => {
                self.next();
                let expr = self.parse_term();
                Expr::UnaryOp(Box::new(expr), op)
            }
            t => panic!("Unexpected token {:?}.", t)
        }
    }
}

pub fn parse(tokens : Vec<Token>) -> Vec<Stmt> {
    let mut parser = Parser { current: Token::None, index: 0, tokens };
    let mut stmts = Vec::new();
    while parser.next().is_some() {
        stmts.push(parser.parse_stmt());
    }
    stmts
}
