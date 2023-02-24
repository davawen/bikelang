use std::fmt::Display;

use itertools::Itertools;

use crate::typed::Type;

use super::{Ast, Node, Intrisic};

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const WHITE: &str = "\x1b[0m";
        const RED: &str = "\x1b[31m";
        const GREEN: &str = "\x1b[32m";
        const YELLOW: &str = "\x1b[33m";
        const BLUE: &str = "\x1b[34m";
        const PINK: &str = "\x1b[35m";
        const CYAN: &str = "\x1b[36m";
        const PURPLE: &str = "\x1b[38;5;177m";
        const ORANGE: &str = "\x1b[38;5;215m";
        const LIGHT_GRAY: &str = "\x1b[37m";
        const GRAY: &str = "\x1b[90m";

        let (name, inner ) = match &self.node {
            Node::Number(n, ..) => ( format!("{ORANGE}NUMBER {n}"), None ),
            Node::Identifier(name, ..) => ( format!("IDENT {name}"), None ),
            Node::StringLiteral(v) => ( format!("{GREEN}LITERAL"), Some(vec![format!("{GREEN}\"{v}\"{WHITE}\n")]) ),
            Node::Definition { typename, name } => ( format!("DEF {name}: {CYAN}{typename:?}"), None ),
            Node::Break => ( "BREAK".to_string(), None ),
            Node::Empty => ( "EMPTY".to_string(), None ),
            Node::Return(box expr, ..) => ( format!("{PURPLE}RETURN"), Some(vec![format!("{expr}")]) ),
            Node::Statement(inner) => {
                ( "STATEMENT".to_string(), Some(vec![format!("{inner}")]) )
            }
            Node::Block(inner, ..) => {
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
            Node::Call { name, parameter_list, .. } => {
                let call = format!("{BLUE}CALL {name}");
                let params = parameter_list.iter().map(|p| format!("ARG {p}")).collect_vec();
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
            Node::If { condition, body } => {
                let base = format!("{PURPLE}IF");
                let condition = format!("CONDITION {condition}");
                let body = format!("BODY {body}");
                ( base, Some(vec![condition, body]) )
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
                let first = it.next().unwrap();

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
