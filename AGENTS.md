# About

Electra is a program for stacking astrophotographic images of deep sky objects.
It is a single GTK 3 desktop application via `gtk3-rs` crate.

The internal/calibr value-bitstream format implemented in `src/compression.rs` is frozen for
backward compatibility with saved files; changes must keep `compression::test_bitstream_compatibility` passing.
