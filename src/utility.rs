use std::fmt::Display;

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

pub trait Inspect where Self: Sized {
    type Error;

    fn my_inspect_err<F: FnOnce(&Self::Error)>(self, closure: F) -> Self;
    fn log_err(self) -> Self;
}

impl<T, E: Display> Inspect for Result<T, E> {
    type Error = E;

    fn my_inspect_err<F: FnOnce(&Self::Error)>(self, closure: F) -> Self {
        if let Err(e) = &self {
            closure(e);
        }

        self
    } 

    fn log_err(self) -> Self {
        self.my_inspect_err(|e| eprintln!("{e}"))
    }
}

pub trait Transmit<T> where Self: Sized {
    fn transmit(self) -> T;
}

impl<T1, T2, E1, E2> Transmit<Result<T2, E2>> for Result<T1, E1> where
    E1: Into<E2>,
    T1: Into<T2>
{
    fn transmit(self) -> Result<T2, E2> {
        self.map_err(|e| e.into()).map(|x| x.into())
    }
}
