use std::default::default;

use indexmap::IndexMap;

use crate::typed::Type;

pub type ScopeIndex = usize;
pub type VariableIndex = usize;

/// Scopes are stored in `analysis::Function`
#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub variables: IndexMap<String, Type>,
}

#[derive(Debug)]
pub struct ScopeStack(Vec<Scope>);

impl ScopeStack {
    fn top(&self) -> &Scope {
        self.0.last().unwrap()
    }

    /// Iterates over every scope in the stack, from top to bottom
    fn iter(&self) -> impl ExactSizeIterator<Item = &Scope> + '_ {
        self.0.iter().rev()
    }

    /// Clones the given parameters
    pub fn from_top(top: Scope) -> Self {
        Self(vec![top]) 
    }

    pub fn push(&mut self, scope: Scope) {
        self.0.push(scope)
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    /// Searches up the scope-tree to see if a variable exists
    pub fn has(&self, name: &str) -> bool {
        for scope in self.iter() {
            if scope.variables.contains_key(name) {
                return true;
            }
        }
        false
    }

    /// Searches up the scope tree to get a variable
    pub fn get<'a>(&'a self, name: &str) -> Option<&'a Type> {
        for scope in self.iter() {
            if let Some(v) = scope.variables.get(name) {
                return Some(v);
            }
        }
        None
    }

    /// Gets the variable at the given index in the last scope
    /// # Panics
    /// This function panics if the given index isn't valid
    /// This function panics if the scope stack is empty
    pub fn get_idx(&self, var: VariableIndex) -> &Type {
        self.top()
            .variables.get_index(var)
            .unwrap().1
    }

    /// Inserts a variable into the last scope
    /// Returns the index of the newly inserted variable
    /// If you want to know wether a variable already exists, use `has`.
    pub fn insert(&mut self, name: String, ty: Type) -> VariableIndex {
        self.top()
            .variables.insert_full(name, ty)
            .0
    }
}
