use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};

#[derive(Debug)]
pub(super) struct Allocator {
    free_locs: BinaryHeap<Reverse<u32>>,
    max_loc: u32,
}

impl Allocator {
    pub(super) fn new() -> Self {
        Allocator {
            free_locs: BinaryHeap::new(),
            max_loc: 0,
        }
    }

    pub(super) fn alloc(&mut self) -> u32 {
        match self.free_locs.pop() {
            None => {
                self.max_loc += 1;
                self.max_loc - 1
            }
            Some(Reverse(loc)) => loc,
        }
    }

    pub(super) fn dealloc_set(&mut self, locs: HashSet<u32>) {
        for loc in locs {
            self.free_locs.push(Reverse(loc))
        }
    }
}
