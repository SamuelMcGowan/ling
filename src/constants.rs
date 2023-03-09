use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub(crate) struct ConstIdx(pub(crate) usize);

#[derive(Default, Debug)]
pub(crate) struct ConstantPool(Vec<Value>);

impl ConstantPool {
    pub fn push(&mut self, value: Value) -> ConstIdx {
        let idx = ConstIdx(self.0.len());
        self.0.push(value);
        idx
    }

    pub fn get(&self, index: ConstIdx) -> Option<&Value> {
        self.0.get(index.0)
    }

    pub fn get_mut(&mut self, index: ConstIdx) -> Option<&mut Value> {
        self.0.get_mut(index.0)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.0.iter()
    }
}
