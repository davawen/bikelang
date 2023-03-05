use std::default::default;

use indexmap::IndexMap;

use crate::typed::Type;

pub type ScopeIndex = usize;
pub type VariableIndex = usize;

/// Scopes are stored in `analysis::Function`
#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: IndexMap<String, Type>,
    pub parent: Option<ScopeIndex>,
    pub childs: Vec<ScopeIndex>,
    /// Size in bytes this scope occupies
    pub size: u32,
    /// Offset from the base of the stack (sum of the size of previous scopes)
    pub offset: u32
}

impl Scope {
    pub fn new() -> Self {
        Self { variables: default(), parent: None, childs: vec![], size: 0, offset: 0 }
    }

    pub fn with_parent(parent: ScopeIndex, scopes: &[Scope]) -> Self {
        let scope = &scopes[parent];
        Self {
            parent: Some(parent),
            offset: scope.offset + scope.size,
            ..Self::new()
        }
    }

    /// Searches up the scope-tree to see if a variable exists
    pub fn has(&self, name: &str, scopes: &[Scope]) -> bool {
        self.variables.contains_key(name)
            || self.parent.map_or(false, |parent| {
                scopes[parent].has(name, scopes)
            })
    }

    /// Searches up the scope tree to get a variable
    pub fn get<'a>(&'a self, name: &str, scopes: &'a [Scope]) -> Option<&'a Type> {
        self.variables.get(name)
            .or(self.parent.map(|parent| {
                scopes[parent].get(name, scopes)
            }).flatten())
    }

    /// # Panics
    /// This function panics if the given index isn't valid
    pub fn get_idx(&self, var: VariableIndex) -> &Type {
        self.variables.get_index(var).unwrap().1
    }

    /// Returns the index of the newly inserted variable
    /// If you want to know wether a variable already exists, use `has`.
    pub fn insert(&mut self, name: String, ty: Type) -> VariableIndex {
        let new_size = ty.size();
        let (idx, old_val) = self.variables.insert_full(name, ty);
        old_val.inspect(|x| {
            let old_size = x.size();

            self.size -= old_size;
            self.offset -= old_size;
        });

        self.size += new_size;
        self.offset += new_size;

        idx
    }
}
