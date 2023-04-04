use super::array::{Array, ArrayValue};

impl<T: ArrayValue> Array<T> {
    pub fn reduce(mut self, identity: T, f: impl Fn(T, T) -> T) -> Self {
        match self.shape.len() {
            0 => self,
            1 => self
                .data
                .iter()
                .cloned()
                .reduce(f)
                .unwrap_or(identity)
                .into(),
            _ => {
                let row_len: usize = self.row_len();
                if self.len() == 0 {
                    self.shape.remove(0);
                    self.data = vec![identity; row_len];
                    return self;
                }
                for i in 1..self.len() {
                    let start = i * row_len;
                    for j in 0..row_len {
                        self.data[j] = f(self.data[j].clone(), self.data[start + j].clone());
                    }
                }
                self.data.truncate(row_len);
                self.shape.remove(0);
                self
            }
        }
    }
}
