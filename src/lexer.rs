#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,

    And,
    Or,
    XOr,
    Not,
    LShift,
    RShift,

    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual
}

impl Operator {
    pub fn from_char(c : char) -> Operator {
        match c {
            '+' => Operator::Add,
            '-' => Operator::Sub,
            '*' => Operator::Mul,
            '/' => Operator::Div,
            '&' => Operator::And,
            '|' => Operator::Or,
            '^' => Operator::XOr,
            '!' => Operator::Not,
            _ => panic!("Invalid operator {}.", c)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    None,
    Identifier(String),
    Integer(i32),

    SemiColon,
    Equals,

    Operator(Operator),

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,

    Let,
    Ret,

    If,
    Elif,
    Else
}

struct Lexer {
    current : char,
    index : usize,
    source : String
}

impl Lexer {
    fn next(&mut self) -> Option<char> {
        let c = self.source.chars().nth(self.index);
        self.current = c.unwrap_or('\0');
        self.index += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.index)
    }
}

pub fn tokenize(source : &str) -> Vec<Token> {
    let mut lexer = Lexer { current: '\0', index: 0, source: source.to_string() };
    let mut tokens = Vec::new();
    while lexer.next().is_some() {
        match lexer.current {
            c if c.is_whitespace() => continue,

            ';' => tokens.push(Token::SemiColon),
            '=' => if let Some('=') = lexer.peek() {
                lexer.next();
                tokens.push(Token::Operator(Operator::Equal));
            } else {
                tokens.push(Token::Equals);
            },
            '+' | '-' | '*' |
            '&' | '|' | '^' | '!' => tokens.push(Token::Operator(Operator::from_char(lexer.current))),
            '/' => {
                match lexer.peek() {
                    Some('/') => {
                        loop {
                            if let Some('\n') = lexer.next() {
                                break;
                            }
                        }
                    },
                    _ => tokens.push(Token::Operator(Operator::from_char(lexer.current)))
                }
            }
            '<' => {
                match lexer.peek() {
                    Some('<') => {
                        lexer.next();
                        tokens.push(Token::Operator(Operator::LShift));
                    }
                    Some('=') => {
                        lexer.next();
                        tokens.push(Token::Operator(Operator::LessEqual));
                    }
                    _ => tokens.push(Token::Operator(Operator::Less))

                }
            }
            '>' => {
                match lexer.peek() {
                    Some('>') => {
                        lexer.next();
                        tokens.push(Token::Operator(Operator::RShift));
                    }
                    Some('=') => {
                        lexer.next();
                        tokens.push(Token::Operator(Operator::GreaterEqual));
                    }
                    _ => tokens.push(Token::Operator(Operator::Greater))
                }
            }

            '(' => tokens.push(Token::OpenParen),
            ')' => tokens.push(Token::CloseParen),
            '{' => tokens.push(Token::OpenBrace),
            '}' => tokens.push(Token::CloseBrace),

            c if c.is_digit(10) => {
                let mut buf = c.to_string();
                while lexer.peek().is_some() {
                    match lexer.peek().unwrap() {
                        c if c.is_digit(10) => buf.push(lexer.next().unwrap()),
                        _ => break
                    }
                }
                tokens.push(Token::Integer(buf.parse().unwrap()));
            }
            c if c.is_ascii_alphabetic() => {
                let mut buf = c.to_string();
                while lexer.peek().is_some() {
                    match lexer.peek().unwrap() {
                        c if c.is_ascii_alphanumeric() => buf.push(lexer.next().unwrap()),
                        _ => break
                    }
                }
                tokens.push(match buf.as_str() {
                    "ret" => Token::Ret,
                    "let" => Token::Let,
                    "if" => Token::If,
                    "elif" => Token::Elif,
                    "else" => Token::Else,
                    _ => Token::Identifier(buf)
                });
            }
            c => panic!("Unrecognized character {}.", c)
        }
    }
    tokens
}
