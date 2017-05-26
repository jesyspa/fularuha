#!/bin/bash

# Compile, link, and run.

rm temp_files/*
cp "$1" temp_files/input.flrh
if [[ -a temp_files/input.flrh ]]; then
    cd compiler
    cabal run -- -i ../temp_files/input.flrh -o ../temp_files/bytecode.flbc
    cd ..
fi
if [[ -a temp_files/bytecode.flbc ]]; then
    cd assembler
    cargo run -- ../temp_files/bytecode.flbc -o ../temp_files/code.json
    cd ..
fi
if [[ -a temp_files/code.json ]]; then
    cd runtime
    cargo run -- ../temp_files/code.json
    cd ..
fi
