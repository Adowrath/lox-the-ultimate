//! Miscellaneous utility functionality.
//!
//! Specifically, nothing in here is Lox-related, even though
//! it might be useful outside of this crate.
use macro_pub::macro_pub;

/// Creates a simple HashMap from the given key-value expressions.
#[macro_pub]
macro_rules! map {
    ($($key:expr => $value:expr),* $(,)?) => {
        let mut map = HashMap::new();
        $(map.insert($key, $value);)*
        map
    }
}
