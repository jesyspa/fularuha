#!/bin/bash

# Compile, link, and run.

cp "$1" temp_files/input.flrh
cd compiler
cabal run -- -i ../temp_files/input.flrh -o ../temp_files/bytecode.flbc
cd ..
cd assembler
cargo run -- ../temp_files/bytecode.flbc -o ../temp_files/code.json
cd ..
cd runtime
cargo run -- ../temp_files/code.json
