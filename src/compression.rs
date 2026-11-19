use bitstream_io::{BitRead, BitWrite};
#[cfg(test)]
use bitstream_io::{BigEndian, BitReader, BitWriter};

// Block format (frozen for backward compatibility, guarded by test_bitstream_compatibility):
// per block of COMPR_BUF_SIZE XOR-deltas a 3-bit header picks a mode; zero-count parameters
// and an optional large-value mask (one bit per value) precede the bit-packed values.
const COMPR_BUF_SIZE:  usize = 64;
const COMPR_BUF_BITS:  u32   = COMPR_BUF_SIZE as u32;
const MAX_LARGE_CNT:   usize = COMPR_BUF_SIZE / 4;

const HEADER_ZEROS:     u32 = 0b000;
const HEADER_NO_COMPR:  u32 = 0b001;
const HEADER_LZ:        u32 = 0b010;
const HEADER_LZ_2:      u32 = 0b011;
const HEADER_LZ_TZ:     u32 = 0b100;
const HEADER_LZ_TZ_2:   u32 = 0b101;

// Must be appended after the final flush: BitWriter discards an incomplete trailing byte
// when dropped, so without this marker the last block's trailing bits would be lost.
pub fn write_end_marker<T: BitWrite>(writer: &mut T) -> std::io::Result<()> {
    writer.write_unsigned::<32, u32>(0)
}

pub struct ValuesCompressor {
    data:       [u32; COMPR_BUF_SIZE],
    data_ptr:   usize,
    prev_value: u32,
}

// Compression for u32, i32 and f32 values
impl ValuesCompressor {
    pub fn new() -> Self {
        Self {
            data:       [0_u32; COMPR_BUF_SIZE],
            data_ptr:   0,
            prev_value: 0,
        }
    }

    pub fn write_u32<T: BitWrite>(&mut self, value: u32, writer: &mut T) -> std::io::Result<()> {
        self.data[self.data_ptr] = value ^ self.prev_value;
        self.prev_value = value;
        self.data_ptr += 1;
        if self.data_ptr == COMPR_BUF_SIZE {
            self.flush(writer)?;
        }
        Ok(())
    }

    pub fn write_f32<T: BitWrite>(&mut self, value: f32, writer: &mut T) -> std::io::Result<()> {
        self.write_u32(value.to_bits(), writer)
    }

    pub fn write_i32<T: BitWrite>(&mut self, value: i32, writer: &mut T) -> std::io::Result<()> {
        let is_neg = value < 0;
        let u32_value = if is_neg {
            ((value as u32 ^ 0xFFFF_FFFF) << 1) | 1
        } else {
            (value as u32) << 1
        };
        self.write_u32(u32_value, writer)
    }

    pub fn flush<T: BitWrite>(&mut self, writer: &mut T) -> std::io::Result<()> {
        if self.data_ptr == 0 {
            return Ok(())
        }
        self.data[self.data_ptr..].fill(0);
        self.data_ptr = 0;
        self.compress(writer)?;
        Ok(())
    }

    fn compress<T: BitWrite>(&mut self, writer: &mut T) -> std::io::Result<()> {
        let mut lz_freq = [0_u8; 32+1];
        let mut lz_values = [0_u8; COMPR_BUF_SIZE];
        let mut lz_large = 32_u32;
        let mut min_tz = 32_u32;
        for (d, lz) in self.data.iter().zip(&mut lz_values) {
            let tz = u32::trailing_zeros(*d);
            min_tz = u32::min(min_tz, tz);
            let l = u32::leading_zeros(*d);
            *lz = l as u8;
            lz_large = u32::min(lz_large, l);
            lz_freq[l as usize] += 1;
        }
        if min_tz == 32 {
            writer.write_unsigned::<3, u32>(HEADER_ZEROS)?;
            return Ok(())
        }
        let mut cnt_sum = 0_usize;
        let mut lz_norm = 0_u32;
        for (lz, cnt) in lz_freq.iter().enumerate() {
            cnt_sum += *cnt as usize;
            if cnt_sum >= MAX_LARGE_CNT {
                lz_norm = lz as u32;
                break;
            }
        }
        lz_norm = u32::min(u32::max(lz_norm, lz_large), 31 - min_tz);

        if lz_norm + min_tz < 2 {
            writer.write_unsigned::<3, u32>(HEADER_NO_COMPR)?;
            for v in self.data {
                writer.write_unsigned::<32, u32>(v)?;
            }
            return Ok(());
        }
        let use_large_lz_bits = lz_norm >= lz_large + 2;
        if min_tz == 0 {
            if !use_large_lz_bits {
                writer.write_unsigned::<3, u32>(HEADER_LZ)?;
                writer.write_unsigned::<5, u32>(lz_large)?;
            } else {
                writer.write_unsigned::<3, u32>(HEADER_LZ_2)?;
                writer.write_unsigned::<5, u32>(lz_large)?;
                writer.write_unsigned::<5, u32>(lz_norm)?;
            }
        } else {
            if !use_large_lz_bits {
                writer.write_unsigned::<3, u32>(HEADER_LZ_TZ)?;
                writer.write_unsigned::<5, u32>(lz_large)?;
                writer.write_unsigned::<5, u32>(min_tz)?;
            } else {
                writer.write_unsigned::<3, u32>(HEADER_LZ_TZ_2)?;
                writer.write_unsigned::<5, u32>(lz_large)?;
                writer.write_unsigned::<5, u32>(lz_norm)?;
                writer.write_unsigned::<5, u32>(min_tz)?;
            }
        }
        let large_len = u32::max(32 - lz_large - min_tz, 1);
        if use_large_lz_bits {
            let mut large_bits = 0_u64;
            for lz in &lz_values {
                large_bits <<= 1;
                if (*lz as u32) < lz_norm {
                    large_bits |= 1;
                }
            }
            writer.write_unsigned::<COMPR_BUF_BITS, u64>(large_bits)?;
            let norm_len = u32::max(32 - lz_norm - min_tz, 1);
            for (v, lz) in self.data.iter().zip(&lz_values) {
                let len_to_write = if (*lz as u32) >= lz_norm { norm_len } else { large_len };
                writer.write_unsigned_var(len_to_write, *v >> min_tz)?;
            }
        } else {
            for v in self.data {
                writer.write_unsigned_var(large_len, v >> min_tz)?;
            }
        }
        Ok(())
    }
}

// Decompression for u32, i32 and f32 values
pub struct ValuesDecompressor {
    values:     [u32; COMPR_BUF_SIZE],
    values_ptr: usize,
    prev_value: u32,
}

impl ValuesDecompressor {
    pub fn new() -> Self {
        Self {
            values:     [0_u32; COMPR_BUF_SIZE],
            values_ptr: COMPR_BUF_SIZE,
            prev_value: 0,
        }
    }

    pub fn read_f32<T: BitRead>(&mut self, reader: &mut T) -> std::io::Result<f32> {
        Ok(f32::from_bits(self.read_u32(reader)?))
    }

    pub fn read_i32<T: BitRead>(&mut self, reader: &mut T) -> std::io::Result<i32> {
        let mut u32_value = self.read_u32(reader)?;
        let neg_flag = u32_value & 1 != 0;
        u32_value >>= 1;
        if neg_flag {
            u32_value ^= 0xFFFF_FFFF;
        }
        Ok(u32_value as i32)
    }

    pub fn read_u32<T: BitRead>(&mut self, reader: &mut T) -> std::io::Result<u32> {
        if self.values_ptr == COMPR_BUF_SIZE {
            self.decompress_values(reader)?;
        }
        let result = self.values[self.values_ptr];
        self.values_ptr += 1;
        Ok(result)
    }

    fn decompress_values<T: BitRead>(&mut self, reader: &mut T) -> std::io::Result<()> {
        self.values_ptr = 0;
        let header = reader.read_unsigned::<3, u32>()?;
        if header == HEADER_ZEROS {
            self.values.fill(self.prev_value);
            return Ok(());
        }
        if header == HEADER_NO_COMPR {
            for v in &mut self.values {
                self.prev_value ^= reader.read_unsigned::<32, u32>()?;
                *v = self.prev_value;
            }
            return Ok(());
        }
        let (lz_large, lz_norm, tz, use_large_lz_bits) = match header {
            HEADER_LZ =>
                (reader.read_unsigned::<5, u32>()?, 0, 0, false),
            HEADER_LZ_2 =>
                (reader.read_unsigned::<5, u32>()?, reader.read_unsigned::<5, u32>()?, 0, true),
            HEADER_LZ_TZ =>
                (reader.read_unsigned::<5, u32>()?, 0, reader.read_unsigned::<5, u32>()?, false),
            HEADER_LZ_TZ_2 => (
                reader.read_unsigned::<5, u32>()?, reader.read_unsigned::<5, u32>()?,
                reader.read_unsigned::<5, u32>()?, true),
            _ =>
            unreachable!(),
        };
        let large_len = u32::max(32 - lz_large - tz, 1);
        if use_large_lz_bits {
            let norm_len = u32::max(32 - lz_norm - tz, 1);
            let mut large_bits = reader.read_unsigned::<COMPR_BUF_BITS, u64>()?;
            for v in &mut self.values {
                // Top mask bit set means this value was stored with large_len
                let len = if (large_bits as i64) < 0 { large_len } else { norm_len };
                large_bits <<= 1;
                self.prev_value ^= reader.read_unsigned_var::<u32>(len)? << tz;
                *v = self.prev_value;
            }
        } else {
            for v in &mut self.values {
                self.prev_value ^= reader.read_unsigned_var::<u32>(large_len)? << tz;
                *v = self.prev_value;
            }
        }
        Ok(())
    }
}

#[test]
fn test_i32_compression() {
    let mut mem_data = Vec::<u8>::new();
    let mem_writer = std::io::Cursor::new(&mut mem_data);
    let mut bit_writer = BitWriter::endian(mem_writer, BigEndian);
    let mut compressor = ValuesCompressor::new();

    compressor.write_i32(-10, &mut bit_writer).unwrap();
    compressor.write_i32(10, &mut bit_writer).unwrap();
    compressor.write_i32(-42, &mut bit_writer).unwrap();
    compressor.write_i32(42, &mut bit_writer).unwrap();

    compressor.write_i32(0, &mut bit_writer).unwrap();
    compressor.write_i32(i32::MIN, &mut bit_writer).unwrap();
    compressor.write_i32(i32::MAX, &mut bit_writer).unwrap();

    compressor.flush(&mut bit_writer).unwrap();
    bit_writer.write_unsigned::<8, u32>(0).unwrap();

    let mem_reader = std::io::Cursor::new(&mem_data);
    let mut bit_reader = BitReader::endian(mem_reader, BigEndian);
    let mut decompressor = ValuesDecompressor::new();

    assert_eq!(decompressor.read_i32(&mut bit_reader).unwrap(), -10);
    assert_eq!(decompressor.read_i32(&mut bit_reader).unwrap(), 10);
    assert_eq!(decompressor.read_i32(&mut bit_reader).unwrap(), -42);
    assert_eq!(decompressor.read_i32(&mut bit_reader).unwrap(), 42);

    assert_eq!(decompressor.read_i32(&mut bit_reader).unwrap(), 0);
    assert_eq!(decompressor.read_i32(&mut bit_reader).unwrap(), i32::MIN);
    assert_eq!(decompressor.read_i32(&mut bit_reader).unwrap(), i32::MAX);
}

// Golden byte sequences captured with bitstream-io 1.x.
// They guarantee that the bitstream format stays compatible
// with files written by previous versions of this code.
#[test]
fn test_bitstream_compatibility() {
    fn hex_to_bytes(s: &str) -> Vec<u8> {
        (0..s.len()).step_by(2)
            .map(|i| u8::from_str_radix(&s[i..i+2], 16).unwrap())
            .collect()
    }
    fn compress_hex<T: Copy>(
        vals: &[T],
        write_val: impl Fn(&mut ValuesCompressor, T, &mut BitWriter<std::io::Cursor<&mut Vec<u8>>, BigEndian>) -> std::io::Result<()>,
    ) -> String {
        let mut mem = Vec::<u8>::new();
        let mut w = BitWriter::endian(std::io::Cursor::new(&mut mem), BigEndian);
        let mut c = ValuesCompressor::new();
        for v in vals { write_val(&mut c, *v, &mut w).unwrap(); }
        c.flush(&mut w).unwrap();
        write_end_marker(&mut w).unwrap();
        mem.iter().map(|b| format!("{:02x}", b)).collect()
    }
    fn decompress_hex<T>(
        s: &str,
        count: usize,
        read_val: impl Fn(&mut ValuesDecompressor, &mut BitReader<std::io::Cursor<&Vec<u8>>, BigEndian>) -> std::io::Result<T>,
    ) -> Vec<T> {
        let data = hex_to_bytes(s);
        let mut r = BitReader::endian(std::io::Cursor::new(&data), BigEndian);
        let mut d = ValuesDecompressor::new();
        (0..count).map(|_| read_val(&mut d, &mut r).unwrap()).collect()
    }

    let g1 = "60ffe00000000000000000000098000000380000023800000038000002a7fffffffc00000000000000000000";
    let i32_vals = [-10, 10, -42, 42, 0, i32::MIN, i32::MAX];
    assert_eq!(compress_hex(&i32_vals, |c, v, w| c.write_i32(v, w)), g1);
    assert_eq!(decompress_hex(g1, i32_vals.len(), |d, r| d.read_i32(r)), i32_vals);

    let g2 = "625bfda492000000000000003c23d70a0200000005615850758561406f1bc700e439080fa9ea600b22c8f70dcc03a4e90805b16c7485210fc370dd485236c5b300e93a41b86e36459701eb7ac1c8723bc6f000d9364d58563e479003d70a3ca3d70f23c92b0ac6c9b21bc6f6721c80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
    let f32_g2: Vec<f32> = (0..32).map(|i| i as f32 / 100.0).collect();
    assert_eq!(compress_hex(&f32_g2, |c, v, w| c.write_f32(v, w)), g2);
    assert_eq!(decompress_hex(g2, f32_g2.len(), |d, r| d.read_f32(r)), f32_g2);

    let g3 = "a1549ea220202000200001fc1fe000800c2003100784c400e26200f098b898803c4c5c4c40fe1317131f131713100f898b898f898b8988000000";
    let f32_g3: Vec<f32> = (0..64).map(|i| i as f32).collect();
    assert_eq!(compress_hex(&f32_g3, |c, v, w| c.write_f32(v, w)), g3);
    assert_eq!(decompress_hex(g3, f32_g3.len(), |d, r| d.read_f32(r)), f32_g3);

    let g4 = "a1549ea2202020002000021c01e000800c2003100784c400e26200f098b898803c4c5c4c40fe1317131f131713100f898b898f898b8988000000";
    let f32_g4: Vec<f32> = (0..64).map(|i| (i * 256) as f32).collect();
    assert_eq!(compress_hex(&f32_g4, |c, v, w| c.write_f32(v, w)), g4);
    assert_eq!(decompress_hex(g4, f32_g4.len(), |d, r| d.read_f32(r)), f32_g4);

    let g5 = "2c204356302f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bd02f324bc605e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e6497a05e64978000000";
    let f32_g5: Vec<f32> = (0..128).map(|i| if i % 2 == 0 { 1.5e20 } else { -7.25e19 }).collect();
    assert_eq!(compress_hex(&f32_g5, |c, v, w| c.write_f32(v, w)), g5);
    assert_eq!(decompress_hex(g5, f32_g5.len(), |d, r| d.read_f32(r)), f32_g5);

    let g6 = "a1549ea2202020002000023c03e000800c2003100784c400e26200f098b898803c4c5c4c40fe1317131f131713100f898b898f898b8988000000";
    let f32_g6: Vec<f32> = (0..64).map(|i| (i * 0x10000) as f32).collect();
    assert_eq!(compress_hex(&f32_g6, |c, v, w| c.write_f32(v, w)), g6);
    assert_eq!(decompress_hex(g6, f32_g6.len(), |d, r| d.read_f32(r)), f32_g6);

    let g7 = "830800000078f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1f8f3942020000006c53b380c53b38143ed08386d637e36af1c187c79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca1010000003629d9c0629d9c0a1f6841c36b1bf1b578fc79ca100000000";
    let f32_g7: Vec<f32> = (0..128).map(|i| (i % 7) as f32 * 1e-20).collect();
    assert_eq!(compress_hex(&f32_g7, |c, v, w| c.write_f32(v, w)), g7);
    assert_eq!(decompress_hex(g7, f32_g7.len(), |d, r| d.read_f32(r)), f32_g7);

    let g8 = "7af04444444444444440e8ee9ee8eebee8ee9ee8eefee8ee9ee8eebee8ee9ee8ee80000000";
    let u32_g8: Vec<u32> = (0..64).collect();
    assert_eq!(compress_hex(&u32_g8, |c, v, w| c.write_u32(v, w)), g8);
    assert_eq!(decompress_hex(g8, u32_g8.len(), |d, r| d.read_u32(r)), u32_g8);

    let g9 = "61fc00000000000000042f6e97900000000000000000000000";
    let f32_g9 = vec![123.456_f32; 100];
    assert_eq!(compress_hex(&f32_g9, |c, v, w| c.write_f32(v, w)), g9);
    assert_eq!(decompress_hex(g9, f32_g9.len(), |d, r| d.read_f32(r)), f32_g9);

    let g10 = "5a8218a39259a7a29aabb2dbafc31cb3d35db7e39ebbf3dfbf8218a39259a7a29aabb2dbafc31cb3d35db7e39ebbf3dfbf00000000";
    let mut u32_g10: Vec<u32> = Vec::new();
    let mut acc = 0_u32;
    for i in 0..64 { acc ^= 32 + (i as u32 % 32); u32_g10.push(acc); }
    assert_eq!(compress_hex(&u32_g10, |c, v, w| c.write_u32(v, w)), g10);
    assert_eq!(decompress_hex(g10, u32_g10.len(), |d, r| d.read_u32(r)), u32_g10);
}

#[test]
fn test_compression_decompression() {
    fn test_values(values: &[f32]) {
        let mut mem_data = Vec::<u8>::new();
        let mem_writer = std::io::Cursor::new(&mut mem_data);
        let mut bit_writer = BitWriter::endian(mem_writer, BigEndian);
        let mut compressor = ValuesCompressor::new();
        for value in values {
            compressor.write_f32(*value, &mut bit_writer).unwrap();
        }
        compressor.flush(&mut bit_writer).unwrap();
        bit_writer.write_unsigned::<8, u32>(0).unwrap();

        let mem_reader = std::io::Cursor::new(&mem_data);
        let mut bit_reader = BitReader::endian(mem_reader, BigEndian);
        let mut decompressor = ValuesDecompressor::new();

        for value in values {
            let decompr_value = decompressor.read_f32(&mut bit_reader).unwrap();
            assert_eq!(*value, decompr_value);
        }
    }

    test_values(&[
        0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09,
        0.10, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19,
        0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29,
        0.30, 0.31
    ]);

    test_values(&[
        0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0,
        10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,
        20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0,
        30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0,
        40.0, 41.0, 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.0, 49.0,
        50.0, 51.0, 52.0, 53.0, 54.0, 55.0, 56.0, 57.0, 58.0, 59.0,
        60.0, 61.0, 62.0, 63.0,
    ]);

    fn test_random_u32() {
        use rand::prelude::*;
        let len = random::<u16>();
        let mut rng = thread_rng();
        let mut values = Vec::new();
        let offset = (random::<u16>() / 8) as u32;
        for _ in 0..len {
            values.push((rng.gen::<u16>() as u32 + offset) as f32);
            if rng.gen::<u16>() & 0xFFF == 0 {
                let eq_len = random::<u16>() / 4;
                let value = rng.gen::<u16>();
                for _ in 0..eq_len {
                    values.push(value as f32);
                }
            }
        }
        test_values(&values);
    }

    fn test_random_u32_f32() {
        use rand::prelude::*;
        let len = random::<u16>();
        let mut rng = thread_rng();
        let mut values = Vec::new();
        for _ in 0..len {
            values.push(rng.gen::<u16>() as f32 / u16::MAX as f32);
            if rng.gen::<u16>() & 0xFFF == 0 {
                let eq_len = random::<u16>() / 4;
                let value = rng.gen::<u16>() as f32 / u16::MAX as f32;
                for _ in 0..eq_len {
                    values.push(value as f32);
                }
            }
        }
        test_values(&values);
    }

    fn test_random_f32() {
        use rand::prelude::*;
        let len = random::<u16>();
        let mut rng = thread_rng();
        let mut values = Vec::new();
        for _ in 0..len {
            values.push(rng.gen::<f32>());
            if rng.gen::<u16>() & 0xFFF == 0 {
                let eq_len = random::<u16>() / 4;
                let value = rng.gen::<f32>();
                for _ in 0..eq_len {
                    values.push(value as f32);
                }
            }
        }
        test_values(&values);
    }

    fn test_random_f32_neg() {
        use rand::prelude::*;
        let len = random::<u16>();
        let mut rng = thread_rng();
        let mut values = Vec::new();
        for _ in 0..len {
            values.push(rng.gen_range(-2.0..2.0));
            if rng.gen::<u16>() & 0xFFF == 0 {
                let eq_len = random::<u16>() / 4;
                let value = rng.gen_range(-10.0..10.0);
                for _ in 0..eq_len {
                    values.push(value as f32);
                }
            }
        }
        test_values(&values);
    }

    for _ in 0..100 {
        test_random_u32();
        test_random_u32_f32();
        test_random_f32();
        test_random_f32_neg();
    }
}
