use crate::constants::ConstantPool;

// TODO: custom `Debug` implementation.
#[derive(Default, Debug)]
pub(crate) struct Chunk {
    constants: ConstantPool,
}
