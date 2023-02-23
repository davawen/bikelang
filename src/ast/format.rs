use std::fmt::Display;

use itertools::Itertools;

use super::{Ast, Node, Intrisic};

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (name, inner ) = match &self.node {
            Node::Number(n, ..) => ( format!("NUMBER {n}"), None ),
            Node::Identifier(name, ..) => ( format!("IDENT {name}"), None ),
            Node::StringLiteral(v) => ( "LITERAL".to_string(), Some(format!("\"{v}\"")) ),
            Node::Definition { typename, name } => ( format!("DEF {name}: {typename:?}"), None ),
            Node::Break => ( "BREAK".to_string(), None ),
            Node::Empty => ( "EMPTY".to_string(), None ),
            Node::Return(box expr, ..) => ( "RETURN".to_string(), Some(format!("{expr}")) ),
            Node::Statement(inner) => {
                ( "STATEMENT".to_string(), Some(format!("{inner}")) )
            }
            Node::Block(inner, ..) => {
                let inner = inner.iter().map(|p| format!("{p}")).join("");
                ( "\x1b[31mBLOCK\x1b[0m".to_string(), Some(inner) )
            }
            Node::Expr { op, lhs, rhs, .. } => {
                let base = format!("\x1b[33mEXPR {op:?}");
                let inner = format!("LHS {lhs}RHS {rhs}");
                ( base, Some(inner) )
            }
            Node::Call { name, parameter_list, .. } => {
                let call = format!("CALL {name}");
                let params = parameter_list.iter().map(|p| format!("ARG {p}")).join("\n");
                ( call, Some(params) )
            }
            Node::Intrisic(i) => match i {
                Intrisic::Asm(_) => ( "INTRISIC ASM".to_string(), None ),
                Intrisic::Print(params) => {
                    let name = "INTRISIC PRINT".to_string();
                    let params = params.iter().map(|p| format!("ARG {p}")).join("\n");
                    ( name, Some(params) )
                }
            }
            Node::If { condition, body } => {
                let base = "IF".to_string();
                let inner = format!("CONDITION {condition}BODY {body}");
                ( base, Some(inner) )
            }
            Node::Loop { body } => {
                let base = "LOOP".to_string();
                ( base, Some(format!("{body}")) )
            }
            Node::FuncDef { name, body, .. } => ( format!("DEFUNC {name}"), Some(format!("{body}")) ),
            _ => unreachable!("{self:#?}")
        };

        writeln!(f, "{name} \x1b[37m@ {}..{}\x1b[0m", self.bounds.start, self.bounds.end)?;
        if let Some(inner) = inner {
            for line in inner.split('\n') {
                if line.is_empty() { continue }
                writeln!(f, "  {line}")?;
            }
        }

        Ok(())
    }
}
