use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ConstIdx(pub(crate) usize);

// TODO: custom `Debug` implementation.
#[derive(Default, Debug)]
pub(crate) struct Chunk {
    constants: Vec<Value>,
}

impl Chunk {
    pub fn add_constant(&mut self, value: Value) -> ConstIdx {
        let idx = ConstIdx(self.constants.len());
        self.constants.push(value);
        idx
    }

    pub fn get_constant(&mut self, idx: ConstIdx) -> Option<&Value> {
        self.constants.get(idx.0)
    }
}
