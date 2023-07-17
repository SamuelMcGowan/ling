use std::collections::{BTreeMap, HashMap};

use serde::{Serialize, Serializer};

/// Serialize a hashmap in an ordered manner.
///
/// https://stackoverflow.com/questions/42723065/how-to-sort-hashmap-keys-when-serializing-with-serde
pub fn ordered_map<K, V, S>(value: &HashMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    for<'a> &'a K: Serialize + Ord,
    for<'a> &'a V: Serialize,
{
    let ordered: BTreeMap<_, _> = value.iter().collect();
    ordered.serialize(serializer)
}
