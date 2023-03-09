use std::{collections::HashMap, default::default};

use slotmap::SlotMap;

use crate::scope::ScopeTrait;

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

#[derive(Default, Debug)]
pub struct Scope {
    pub variables: SlotMap<VariableKey, VariableOffset>,
    pub named_variables: HashMap<String, VariableKey>,
    pub offset: u32,
    pub kind: ScopeKind
}

#[allow(dead_code)]
#[derive(Default, Debug, Clone)]
pub enum ScopeKind {
    #[default]
    Block,
    // If { end_label: LabelIndex },
    Loop { /* start_label: LabelIndex, */ end_label: LabelIndex }
}

impl Scope {
    pub fn with_parent(parent: &Scope, kind: ScopeKind) -> Self {
        Self {
            offset: parent.offset,
            kind,
            ..default()
        }
    }
}

impl ScopeTrait for Scope {
    type VariableType = VariableOffset;
    type Key = VariableKey;

    fn has_variable(&self, name: &str) -> bool {
        self.named_variables.contains_key(name)
    }

    fn get_variable(&self, name: &str) -> Option<&Self::VariableType> {
        let Some(key) = self.named_variables.get(name) else { return None };

        self.variables.get(*key)
    }

    fn insert(&mut self, name: String, var: Self::VariableType) -> Self::Key {
        self.offset += var.size;

        let key = self.variables.insert(var);
        self.named_variables.insert(name, key);
        key
    }

    fn get_index(&self, idx: Self::Key) -> &Self::VariableType {
        &self.variables[idx]
    }
}
