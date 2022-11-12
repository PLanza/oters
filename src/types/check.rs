use std::collections::HashMap;

use super::Type;

pub struct ProgramChecker {
    // These represent the global top-level declarations
    value_dec: HashMap<String, Type>, // Maps top-level values (i.e. `let` exprs) to their type
    type_decs: HashMap<String, Type>, // Maps `type`s, `enum`s and `struct`s to their definitions
    variant_map: HashMap<String, String>, // Maps variants to their corresponding `enum`
}
