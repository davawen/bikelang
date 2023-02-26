use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct Bounds {
    pub start: usize,
    pub end: usize
}

impl Bounds {
    pub const ZERO: Self = Bounds { start: 0, end: 0 };

    pub fn with_start(mut self, start: usize) -> Self {
        self.start = start;
        self
    }

    pub fn with_end(mut self, end: usize) -> Self {
        self.end = end;
        self
    }

    pub fn with_start_of(mut self, bounds: Bounds) -> Self {
        self.start = bounds.start;
        self
    }

    pub fn with_end_of(mut self, bounds: Bounds) -> Self {
        self.end = bounds.end;
        self
    }

    pub fn extend(mut self, bounds: Bounds) -> Self {
        if bounds.start < self.start { self.start = bounds.start }
        if bounds.end > self.end { self.end = bounds.end }
        self
    }
}

impl std::ops::Index<Bounds> for str {
    type Output = str;
    fn index(&self, index: Bounds) -> &Self::Output {
        &self[index.start..index.end]
    }
}

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

pub mod color {
    pub const WHITE: &str = "\x1b[0m";
    pub const RED: &str = "\x1b[31m";
    pub const GREEN: &str = "\x1b[32m";
    pub const YELLOW: &str = "\x1b[33m";
    pub const BLUE: &str = "\x1b[34m";
    pub const PINK: &str = "\x1b[35m";
    pub const CYAN: &str = "\x1b[36m";
    pub const PURPLE: &str = "\x1b[38;5;177m";
    pub const ORANGE: &str = "\x1b[38;5;215m";
    pub const LIGHT_GRAY: &str = "\x1b[37m";
    pub const GRAY: &str = "\x1b[90m";
}

