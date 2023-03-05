use std::{collections::HashMap, default::default};

use slotmap::SlotMap;

use crate::ast;

use super::LabelIndex;

pub type ScopeIndex = usize;
pub type VariableKey = slotmap::DefaultKey;

#[derive(Debug, Clone, Copy)]
pub struct VariableId(pub ScopeIndex, pub VariableKey);

#[derive(Debug, Clone, Copy)]
pub struct VariableOffset {
    /// The size of this variable in bytes
    pub size: u32,
    /// The offset of this variable from the stack base
    pub offset: u32,
    /// Wether this variable is an argument (is it stored in this stack frame or in the parent one)
    pub argument: bool
}

#[derive(Debug)]
pub struct Scope {
    pub variables: SlotMap<VariableKey, VariableOffset>,
    pub named_variables: HashMap<String, VariableKey>,
    pub parent: Option<ScopeIndex>,
    pub childs: Vec<ScopeIndex>,
    pub idx: ScopeIndex,
    pub kind: ScopeKind
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ScopeKind {
    Block,
    // If { end_label: LabelIndex },
    Loop { /* start_label: LabelIndex, */ end_label: LabelIndex }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: default(),
            named_variables: default(),
            parent: None,
            idx: 0,
            childs: vec![],
            kind: ScopeKind::Block
        }
    }

    pub fn from_parameters(scope: ast::scope::Scope, idx: ScopeIndex) -> Self {
        let mut variables = SlotMap::new();
        let mut offset = 16;
        let named_variables = scope.variables.into_iter()
            .map(|(name, ty)| {
                let size = ty.size();
                let key = variables.insert(VariableOffset {
                    size,
                    offset,
                    argument: true
                });
                offset += size;

                (name, key)
            })
            .collect();

        Self {
            variables,
            named_variables,
            parent: scope.parent,
            childs: scope.childs,
            idx,
            kind: ScopeKind::Block
        }
    }

    pub fn from_ast_scope(scope: ast::scope::Scope, idx: ScopeIndex) -> Self {
        let mut variables = SlotMap::new();
        let mut offset = 0;
        let named_variables = scope.variables.into_iter()
            .map(|(name, ty)| {
                let size = ty.size();
                offset += size;
                let key = variables.insert(VariableOffset {
                    size,
                    offset: scope.offset + offset,
                    argument: false
                });

                (name, key)
            })
            .collect();

        Self {
            variables,
            named_variables,
            parent: scope.parent,
            childs: scope.childs,
            idx,
            kind: ScopeKind::Block
        }
    }

    /// Searchs up the scope tree to get the wanted variable
    /// Garanteed by `ast::analysis` to not fail
    pub fn get(&self, name: &str, scopes: &[Scope]) -> VariableId {
        if let Some(key) = self.named_variables.get(name) {
            VariableId(self.idx, *key)
        } else {
            eprintln!("{}, {:?}, {}, {:#?}", self.idx, self.parent, name, self);
            let parent = self.parent.unwrap();
            scopes[parent].get(name, scopes)
        }
    }

    /// Search up for a specific scope kind using the given predicate
    pub fn search<'a, P: Fn(&ScopeKind) -> bool>(&'a self, scopes: &'a [Scope], predicate: P) -> Option<&'a ScopeKind> {
        if predicate(&self.kind) {
            Some(&self.kind)
        } else if let Some(parent) = self.parent {
            scopes[parent].search(scopes, predicate)
        } else {
            None
        }
    }
}
