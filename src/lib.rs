mod ast;
mod lexer;
pub mod parser;
pub use crate::parser::parse;
pub use crate::ast::*;
pub use crate::parser::{IntoStatic};
