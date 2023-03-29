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
    /// Negative means the variable is in the current stack frame, otherwise it's in the parent one
    pub offset: i32,
}

#[derive(Default, Debug)]
pub struct Scope {
    pub variables: SlotMap<VariableKey, VariableOffset>,
    pub named_variables: HashMap<String, VariableKey>,
    /// Always negative
    pub offset: i32,
    /// The maximum amount of space reserved by the scope, may be greater than `offset` if variables are shadowed
    pub max_offset: i32,
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

    fn get_variable(&self, name: &str) -> Option<&Self::VariableType> {
        let Some(key) = self.named_variables.get(name) else { return None };

        self.variables.get(*key)
    }

    fn insert(&mut self, name: String, mut var: Self::VariableType) -> Self::Key {
        if let Some(shadowed) = self.named_variables.get(&name) {
            let shadowed = self.variables.remove(*shadowed).expect("undefined shadowed variable");

            // if variable was shadowed right next to itself -> change the top of the stack however you want
            // let _ a;
            // let _ a;
            if self.offset == shadowed.offset {
                self.offset += shadowed.size as i32;
                self.offset -= var.size as i32;

                var.offset = self.offset;
            }
            // if variable is shadowed later, but fits in the already allocated space
            // let i32 a;
            // let _ b;
            // let i8 a;
            else if var.size <= shadowed.size {
                var.offset = shadowed.offset;
            }
            // variable is shadowed later and doesn't fit in the existing space
            // let i8 a;
            // let _ b;
            // let i32 a;
            else {
                self.offset -= var.size as i32;
            }
            // of course the best would just be to always put the space for the bigger variable, but that's a bit harder
        } else {
            self.offset -= var.size as i32;
        }
        self.max_offset = self.max_offset.min(self.offset);

        let key = self.variables.insert(var);
        self.named_variables.insert(name, key);
        key
    }

    fn get_index(&self, idx: Self::Key) -> &Self::VariableType {
        &self.variables[idx]
    }
}
