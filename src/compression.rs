use bitstream_io::{BitWrite, BitRead};

const F32_COMPR_BUF_SIZE: usize = 8;

pub struct F32Compressor {
    data: [u32; F32_COMPR_BUF_SIZE],
    data_ptr: usize,
    prev: u32,
}

impl F32Compressor {
    pub fn new() -> F32Compressor {
        F32Compressor {
            data: [0_u32; F32_COMPR_BUF_SIZE],
            data_ptr: 0,
            prev: 0,
        }
    }

    pub fn write<T: BitWrite>(&mut self, value: f32, writer: &mut T) -> anyhow::Result<()> {
        let value_u32 = value.to_bits();
        self.data[self.data_ptr] = value_u32 ^ self.prev;
        self.prev = value_u32;
        self.data_ptr += 1;
        if self.data_ptr == F32_COMPR_BUF_SIZE {
            self.flush(writer)?;
        }
        Ok(())
    }

    pub fn flush<T: BitWrite>(&mut self, writer: &mut T) -> anyhow::Result<()> {
        self.data[self.data_ptr..].fill(0);
        let min_lz = self.data
            .iter()
            .map(|v| v.leading_zeros())
            .min()
            .unwrap_or(0);
        let mut max_len = 32-min_lz;
        if max_len == 0 { max_len = 1; }
        writer.write(5, max_len-1)?;
        for v in self.data {
            writer.write(max_len, v)?;
        }
        self.data_ptr = 0;
        Ok(())
    }
}

pub struct F32Decompressor {
    values: [f32; F32_COMPR_BUF_SIZE],
    values_ptr: usize,
    prev_value: u32,
}

impl F32Decompressor {
    pub fn new() -> F32Decompressor {
        F32Decompressor {
            values: [0_f32; F32_COMPR_BUF_SIZE],
            values_ptr: F32_COMPR_BUF_SIZE,
            prev_value: 0,
        }
    }

    pub fn get<T: BitRead>(&mut self, reader: &mut T) -> anyhow::Result<f32> {
        if self.values_ptr == F32_COMPR_BUF_SIZE {
            self.values_ptr = 0;
            let len = reader.read::<u32>(5)? + 1;
            for v in self.values.iter_mut() {
                self.prev_value ^= reader.read::<u32>(len)?;
                *v = f32::from_bits(self.prev_value)
            }
        }
        let result = self.values[self.values_ptr];
        self.values_ptr += 1;
        Ok(result)
    }
}
