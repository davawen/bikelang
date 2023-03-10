pub trait ScopeTrait {
    type VariableType;
    type Key;

    fn get_variable(&self, name: &str) -> Option<&Self::VariableType>;
    fn get_index(&self, idx: Self::Key) -> &Self::VariableType;
    fn insert(&mut self, name: String, var: Self::VariableType) -> Self::Key;
}
#[derive(Debug)]
pub struct ScopeStack<T: ScopeTrait>(Vec<T>);

impl<T: ScopeTrait> ScopeStack<T> {
    /// Iterates over every scope in the stack, from top to bottom
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> + '_ {
        self.0.iter().rev()
    }

    pub fn top(&self) -> &T {
        self.0.last().unwrap()
    }

    pub fn top_mut(&mut self) -> &mut T {
        self.0.last_mut().unwrap()
    }

    pub fn at(&self, idx: usize) -> &T {
        &self.0[idx]
    }

    /// Clones the given parameters
    pub fn from_top(top: T) -> Self {
        Self(vec![top]) 
    }

    pub fn push(&mut self, scope: T) {
        self.0.push(scope)
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    /// Searches up the scope tree to get a variable
    pub fn get<'a>(&'a self, name: &str) -> Option<&'a T::VariableType> {
        for scope in self.iter() {
            if let Some(v) = scope.get_variable(name) {
                return Some(v);
            }
        }
        None
    }

    /// Gets the variable at the given index in the last scope
    /// # Panics
    /// This function panics if the given index isn't valid
    /// This function panics if the scope stack is empty
    pub fn get_index(&self, idx: T::Key) -> &T::VariableType {
        self.top().get_index(idx)
    }

    /// Inserts a variable into the last scope
    /// Returns the index of the newly inserted variable
    /// If you want to know wether a variable already exists, use `has`.
    pub fn insert(&mut self, name: String, var: T::VariableType) -> T::Key {
        self.top_mut().insert(name, var)
    }
}
