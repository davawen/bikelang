pub trait PushIndex {
    type Item;
    fn push_idx(&mut self, value: Self::Item) -> usize;
}

impl<T> PushIndex for Vec<T> {
    type Item = T;
    fn push_idx(&mut self, value: Self::Item) -> usize {
        let idx = self.len();
        self.push(value);
        idx
    }
}
