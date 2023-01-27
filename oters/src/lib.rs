#![allow(dead_code)]

use anyhow::Result;
use types::check::ProgramChecker;

pub mod export;
pub mod exprs;
pub mod interpret;
pub mod parser;
pub mod types;

pub fn load_std_lib(checker: &mut ProgramChecker) -> Result<()> {
    let std_lib = parser::parse_source(include_str!("std.otrs").to_string())?;
    checker.type_check_program(&std_lib, vec!["std".to_string()])?;
    Ok(())
}
