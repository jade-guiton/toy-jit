#!/bin/bash
cd gdb_sym_reader
cargo build
sudo mkdir -p /usr/lib/gdb
sudo cp target/debug/librust_jit_gdb_reader.so /usr/lib/gdb/rust-jit
