#![allow(dead_code)]

use anyhow::Result;
use types::check::ProgramChecker;

pub mod errors;
pub mod export;
pub mod exprs;
pub mod interpret;
pub mod parser;
pub mod std;
pub mod types;

pub use oters_macro::run;

pub use crate::export::{ExportFns, Value};
pub use crate::types::Type;

use crate as oters;
use crate::std::*;
oters::export::export_list!();

pub fn load_std_lib(checker: &mut ProgramChecker) -> Result<oters::export::PathExportFns> {
    let std_src = include_str!("std.otrs").to_string();
    let std_lib = parser::parse_source(std_src.clone()).map_err(|e| e.to_anyhow(&std_src))?;
    checker
        .type_check_program(
            &std_lib,
            vec!["std".to_string()],
            Some((
                EXPORT_FNS.clone(),
                EXPORT_STRUCTS.clone(),
                EXPORT_ENUMS.clone(),
            )),
        )
        .map_err(|e| e.to_anyhow(&std_src))?;

    let stream_src = include_str!("std/stream.otrs").to_string();
    let stream = parser::parse_source(stream_src.clone()).map_err(|e| e.to_anyhow(&stream_src))?;
    checker
        .type_check_program(&stream, vec!["std".to_string(), "stream".to_string()], None)
        .map_err(|e| e.to_anyhow(&stream_src))?;

    let event_src = include_str!("std/event.otrs").to_string();
    let event = parser::parse_source(event_src.clone()).map_err(|e| e.to_anyhow(&event_src))?;
    checker
        .type_check_program(&event, vec!["std".to_string(), "event".to_string()], None)
        .map_err(|e| e.to_anyhow(&event_src))?;

    let path_export_fns: oters::export::PathExportFns = EXPORT_FNS
        .iter()
        .map(|(name, vals)| ((vec!["std".to_string()], name.clone()), vals.clone()))
        .collect();

    Ok(path_export_fns)
}
