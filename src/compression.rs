use bitstream_io::{BitWrite, BitRead};

const F32_COMPR_BUF_SIZE: usize = 8;

pub struct ValuesCompressor {
    data: [u32; F32_COMPR_BUF_SIZE],
    data_ptr: usize,
    prev: u32,
}

impl ValuesCompressor {
    pub fn new() -> Self {
        Self {
            data: [0_u32; F32_COMPR_BUF_SIZE],
            data_ptr: 0,
            prev: 0,
        }
    }

    pub fn write_u32<T: BitWrite>(&mut self, value: u32, writer: &mut T) -> std::io::Result<()> {
        self.data[self.data_ptr] = value ^ self.prev;
        self.prev = value;
        self.data_ptr += 1;
        if self.data_ptr == F32_COMPR_BUF_SIZE {
            self.flush(writer)?;
        }
        Ok(())
    }

    pub fn write_f32<T: BitWrite>(&mut self, value: f32, writer: &mut T) -> std::io::Result<()> {
        self.write_u32(value.to_bits(), writer)
    }

    pub fn flush<T: BitWrite>(&mut self, writer: &mut T) -> std::io::Result<()> {
        if self.data_ptr == 0 {
            return Ok(())
        }
        self.data[self.data_ptr..].fill(0);
        let min_lz = self.data
            .iter()
            .map(|v| v.leading_zeros())
            .min()
            .unwrap_or(0);

        let min_tz = self.data
            .iter()
            .map(|v| v.trailing_zeros())
            .min()
            .unwrap_or(0);

        let mut max_len = 32-min_lz.max(min_tz);
        if max_len == 0 { max_len = 1; }

        if min_lz >= min_tz {
            writer.write(6, max_len-1)?;
            for v in self.data {
                writer.write(max_len, v)?;
            }
        } else {
            writer.write(6, 32|(max_len-1))?;
            for v in self.data {
                writer.write(max_len, v >> min_tz)?;
            }
        }
        self.data_ptr = 0;
        Ok(())
    }
}

pub struct ValuesDecompressor {
    values: [u32; F32_COMPR_BUF_SIZE],
    values_ptr: usize,
    prev_value: u32,
}

impl ValuesDecompressor {
    pub fn new() -> Self {
        Self {
            values: [0_u32; F32_COMPR_BUF_SIZE],
            values_ptr: F32_COMPR_BUF_SIZE,
            prev_value: 0,
        }
    }

    pub fn read_f32<T: BitRead>(&mut self, reader: &mut T) -> std::io::Result<f32> {
        Ok(f32::from_bits(self.read_u32(reader)?))
    }

    pub fn read_u32<T: BitRead>(&mut self, reader: &mut T) -> std::io::Result<u32> {
        if self.values_ptr == F32_COMPR_BUF_SIZE {
            self.values_ptr = 0;
            let tz_and_len = reader.read::<u32>(6)?;
            let use_lz = (tz_and_len & 32) == 0;
            let len = (tz_and_len & 31) + 1;
            if use_lz {
                for v in &mut self.values {
                    self.prev_value ^= reader.read::<u32>(len)?;
                    *v = self.prev_value;
                }
            } else {
                let shift = 32-len;
                for v in &mut self.values {
                    self.prev_value ^= reader.read::<u32>(len)? << shift;
                    *v = self.prev_value;
                }
            }
        }
        let result = self.values[self.values_ptr];
        self.values_ptr += 1;
        Ok(result)
    }
}
