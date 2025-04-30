#![feature(box_patterns)]
mod ast;
mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

pub use crate::ast::*;
pub use crate::parser::IntoStatic;
pub use crate::parser::parse;
