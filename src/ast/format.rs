use std::fmt::Display;

use itertools::Itertools;

use crate::{ typed::Type, utility::color::* };

use super::{Ast, Node, Intrisic};

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (name, inner ) = match &self.node {
            Node::Number(n, ..) => ( format!("{ORANGE}NUMBER {n}"), None ),
            Node::Identifier(name, ..) => ( format!("IDENT {name}"), None ),
            Node::StringLiteral(v) => ( format!("{GREEN}LITERAL"), Some(vec![format!("{GREEN}{v:?}{WHITE}\n")]) ),
            Node::BoolLiteral(b) => {
                let b = if *b { "TRUE" } else { "FALSE" };
                ( format!("{ORANGE}{b}"), None )
            }
            Node::Definition { typename, name } => ( format!("DEF {name}: {CYAN}{typename:?}"), None ),
            Node::Break => ( "BREAK".to_string(), None ),
            Node::Empty => ( "EMPTY".to_string(), None ),
            Node::Convert(box expr, ty) => ( format!("CONVERT TO {ty:?}"), Some(vec![format!("{expr}")]) ),
            Node::Return(box expr, ..) => ( format!("{PURPLE}RETURN"), Some(vec![format!("{expr}")]) ),
            Node::Statement(inner) => {
                ( "STATEMENT".to_string(), Some(vec![format!("{inner}")]) )
            }
            Node::Block { inner, .. } => {
                let inner = inner.iter().map(|p| format!("{p}")).collect_vec();
                ( format!("{RED}BLOCK"), Some(inner) )
            }
            Node::Expr { op, lhs, rhs, .. } => {
                let base = format!("{YELLOW}EXPR {op:?}");
                let lhs = format!("LHS {lhs}");
                let rhs = format!("RHS {rhs}");
                ( base, Some(vec![lhs, rhs]) )
            }
            Node::UnaryExpr { op, value, .. } => {
                let base = format!("{YELLOW}EXPR {op:?}");
                let inner = format!("{value}");
                ( base, Some(vec![inner]) )
            }
            Node::Call { name, argument_list, .. } => {
                let call = format!("{BLUE}CALL {name}");
                let params = argument_list.iter().map(|p| format!("ARG {p}")).collect_vec();
                ( call, Some(params) )
            }
            Node::Intrisic(i) => match i {
                Intrisic::Asm(_) => ( format!("{PINK}INTRISIC ASM"), None ),
                Intrisic::Print(params) => {
                    let name = format!("{PINK}INTRISIC PRINT");
                    let params = params.iter().map(|p| format!("ARG {p}")).collect_vec();
                    ( name, Some(params) )
                }
            }
            Node::If { condition, body, else_body, ty: _ } => {
                let base = format!("{PURPLE}IF");
                let condition = format!("CONDITION {condition}");
                let body = format!("BODY {body}");
                let else_body = if let Some(else_body) = else_body {
                    format!("ELSE {else_body}")
                } else { String::new() };
                ( base, Some(vec![condition, body, else_body]) )
            }
            Node::Loop { body } => {
                let base = format!("{PURPLE}LOOP");
                ( base, Some(vec![format!("{body}")]) )
            }
            Node::FuncDef { name, body, .. } => ( format!("\x1b[34mDEFUNC {name}"), Some(vec![format!("{body}")]) ),
        };

        let ty = self.get_type();
        let ty = if ty != Type::Void {
            format!("{WHITE} -> {CYAN}{ty:?}")
        } else {
            String::new()
        };

        writeln!(f, "{name}{ty} {LIGHT_GRAY}@ {}..{}{WHITE}",
            self.bounds.start,
            self.bounds.end
        )?;
        if let Some(inner) = inner {
            for (idx, arm) in inner.iter().enumerate() {
                let is_last = idx == inner.len()-1;

                let mut it = arm.split_inclusive('\n');
                let Some(first) = it.next() else { continue };

                let connector = if is_last { '╰' } else { '├' };
                let separator = if is_last { ' ' } else { '│' };

                write!(f, "{GRAY}{connector} {WHITE}{first}")?;
                for line in it {
                    write!(f, "{GRAY}{separator} {WHITE}{line}")?;
                }
            }
        }

        Ok(())
    }
}
