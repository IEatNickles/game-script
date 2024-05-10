use std::str::FromStr;

use crate::{lexer::Operator, parser::{Expr, IfPredicate, Stmt}};

#[allow(dead_code)]
enum Register {
    RAX,
    RBX,
    RCX,
    RDX,

    RDI,
    RSI,

    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    RBP,
    RSP,

    Stack(u32)
}

impl ToString for Register {
    fn to_string(&self) -> String {
        match self{
            Register::RAX => "rax".to_string(),
            Register::RBX => "rbx".to_string(),
            Register::RCX => "rcx".to_string(),
            Register::RDX => "rdx".to_string(),
            Register::RDI => "rdi".to_string(),
            Register::RSI => "rsi".to_string(),
            Register::R8 =>  "r8" .to_string(),
            Register::R9 =>  "r9" .to_string(),
            Register::R10 => "r10".to_string(),
            Register::R11 => "r11".to_string(),
            Register::R12 => "r12".to_string(),
            Register::R13 => "r13".to_string(),
            Register::R14 => "r14".to_string(),
            Register::R15 => "r15".to_string(),
            Register::RBP => "rbp".to_string(),
            Register::RSP => "rsp".to_string(),
            Register::Stack(loc) => format!("QWORD [rsp + {}]", loc * 8),
        }
    }
}

#[derive(Debug, Clone)]
struct Var {
    stack_loc : u32,
    name : String
}

struct Generator {
    current : Stmt,
    index : usize,
    stmts : Vec<Stmt>,

    variables : Vec<Var>,
    scopes : Vec<u32>,
    stack_ptr : u32,

    result : String,

    label_count : u32
}

impl Generator {
    fn next(&mut self) -> Option<Stmt> {
        let s = self.stmts.get(self.index).cloned();
        self.current = s.clone().unwrap_or(Stmt::None);
        self.index += 1;
        s
    }

    fn gen_statement(&mut self, stmt : Stmt) {
        match stmt {
            Stmt::Return(expr) => {
                self.gen_expr(expr.clone());
                self.pop(Register::RAX);
                self.result.push_str("\tleave\n\tret\n");
            }
            Stmt::Let(ident, expr) => {
                self.comment(&format!("var '{}'", ident));
                self.gen_expr(expr.clone());
                self.add_variable(&ident);
            }
            Stmt::VarAssign(ident, expr) => {
                self.comment(&format!("assign '{}'", ident));
                self.gen_expr(expr.clone());
                self.pop(Register::RAX);
                self.set_variable(&ident);
            }
            Stmt::If(cond, stmt, pred) => {
                let label = self.gen_label();
                self.gen_expr(cond);
                self.pop(Register::RAX);
                self.result.push_str(&format!("\ttest rax, rax\n\tjz .{}\n", label));
                self.gen_statement(*stmt);
                let end_label = { self.gen_label() };
                self.result.push_str(&format!("\tjmp .{}\n.{}:\n", end_label, label));
                if let Some(pred) = pred {
                    self.gen_if_predicate(pred, end_label);
                }
            }
            Stmt::Scope(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.gen_statement(stmt.clone());
                }
                self.end_scope();
            }
            s => panic!("Could not generate statement '{:?}'.", s)
        }
        self.result.push('\n');
    }

    fn gen_if_predicate(&mut self, pred : IfPredicate, end_label : String) {
        let label = self.gen_label();
        self.gen_expr(pred.cond);
        self.pop(Register::RAX);
        self.result.push_str(&format!("\ttest rax, rax\n\tjz .{}\n", label));
        self.gen_statement(*pred.body);
        self.result.push_str(&format!(".{}:\n", label));
        if let Some(pred) = *pred.predicate {
            self.gen_if_predicate(pred, end_label);
        } else {
            self.result.push_str(&format!(".{}:\n", end_label));
        }
    }

    fn gen_expr(&mut self, expr : Expr) {
        match expr.clone() {
            Expr::Integer(num) => {
                self.result.push_str(&format!("\tmov rax, {}\n", num));
                self.push(Register::RAX);
            }
            Expr::VarRef(ident) => {
                self.comment(&format!("ref '{}'", ident));
                self.push_variable(&ident);
            }
            Expr::BinaryOp(lhs, rhs, op) => {
                self.gen_expr(*rhs);
                self.gen_expr(*lhs);
                self.pop(Register::RAX);
                self.pop(Register::RBX);
                match op {
                    Operator::Add => self.result.push_str("\tadd rax, rbx\n"),
                    Operator::Sub => self.result.push_str("\tsub rax, rbx\n"),
                    Operator::Mul => self.result.push_str("\tmul rbx\n"),
                    Operator::Div => self.result.push_str("\tmov rdx, 0\n\tdiv rbx\n"),
                    Operator::And => self.result.push_str("\tand rax, rbx\n"),
                    Operator::Or => self.result.push_str("\tor rax, rbx\n"),
                    Operator::XOr => self.result.push_str("\txor rax, rbx\n"),
                    Operator::LShift => self.result.push_str("\tmov rcx, rbx\n\tsal rax, cl\n"),
                    Operator::RShift => self.result.push_str("\tmov rcx, rbx\n\tsar rax, cl\n"),
                    Operator::Equal => self.result.push_str("\tcmp rax, rbx"),
                    _ => panic!("Invalid binary operator '{:?}'.", op)
                };
                self.push(Register::RAX);
            }
            Expr::UnaryOp(expr, op) => {
                self.gen_expr(*expr);
                self.pop(Register::RAX);
                match op {
                    Operator::Not => self.result.push_str("\tnot rax\n"),
                    _ => panic!("Invalid unary operator '{:?}'.", op)
                }
                self.push(Register::RAX);
            }
            Expr::Scope(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.gen_statement(stmt.clone());
                }
                self.end_scope();
            }
            //e => println!("Expected expression, found {:?}.", e)
        }
        self.result.push('\n');
    }

    fn gen_label(&mut self) -> String {
        self.label_count += 1;
        format!("label{}", self.label_count)
    }

    fn push(&mut self, register : Register) {
        self.stack_ptr += 1;
        self.result.push_str(&format!("\tpush {}\n", register.to_string()));
    }

    fn pop(&mut self, register : Register) {
        self.stack_ptr -= 1;
        self.result.push_str(&format!("\tpop {}\n", register.to_string()));
    }

    fn begin_scope(&mut self) {
        self.scopes.push(self.stack_ptr);
    }

    fn end_scope(&mut self) {
        let count = self.stack_ptr - self.scopes.pop().unwrap();
        if count > 0 {
            self.pop_variables(count);
        }
    }

    fn add_variable(&mut self, name : &str) {
        self.variables.push(Var {name: name.to_string(), stack_loc: self.stack_ptr});
    }

    fn push_variable(&mut self, name : &str) {
        match self.get_variable(name) {
            Ok(v) => self.push(Register::Stack(self.stack_ptr - v.stack_loc)),
            Err(e) => panic!("{}", e)
        }
    }

    fn set_variable(&mut self, name : &str) {
        match self.get_variable(name) {
            Ok(v) => self.result.push_str(&format!("\tmov QWORD [rsp + {}], rax\n", (self.stack_ptr - v.stack_loc) * 8)),
            Err(e) => panic!("{}", e)
        }
    }

    fn get_variable(&self, name : &str) -> Result<Var, String> {
        match self.variables.iter().rev().find(|v| -> bool {v.name == name}).cloned() {
            Some(v) => Ok(v),
            None => Err(format!("Variable '{}' does not exist.", name))
        }
    }

    fn pop_variable(&mut self) {
        self.variables.pop();
        self.stack_ptr -= 1;
    }

    fn pop_variables(&mut self, count : u32) {
        self.result.push_str(&format!("\tadd rsp, {}", count * 8));
        for _ in 0..count {
            self.pop_variable();
        }
    }

    fn comment(&mut self, comment : &str) {
        self.result.push_str(&format!(";; {}\n", comment));
    }
}

pub fn generate(stmts : Vec<Stmt>) -> String {
    let mut generator = Generator {
        current: Stmt::None,
        index: 0, stmts,
        variables: Vec::new(), scopes: Vec::new(), stack_ptr: 0,
        result : String::from_str("global main\nsection .text\nmain:\n\tpush rbp\n\tmov rbp, rsp\n;; Begin\n\n").unwrap(),
        label_count: 0
    };
    while let Some(stmt) = generator.next() {
        generator.gen_statement(stmt);
    }
    generator.result
}
