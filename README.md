# Huffman text compression script

## Authors
- Yehor KOROTENKO (yehor.korotenko@etu-upsaclay.fr)
- Ivan KHARKOV (ivan.kharkov@etu-upsaclay.fr)

## About
The project's goal is to write a CLI tool to compress and decompress files using the OCaml programming language that was learnt on **Introduction to Functional Programming** course taught by the professor **Kim Nguyễn** [link](https://usr.lmf.cnrs.fr/~kn/index_en.html)

## Setup
1. Make sure you have __ocaml__ and __dune__ installed on your machine
2. Clone the project `git clone https://github.com/DobbiKov/project_ipf.git`
3. Enter into the folder `cd projet_ipf`
4. Execute the file, for example to see the help, write: `dune exec ./huff.exe --help`

## Docs
- **bs.ml** - module for reading and writing data to a file
- **heap.ml** - module with min_heap implemenation to reduce complexity of construction a huff_tree
- **read_write_file.ml** - module with functions to read and write files with (de)compression algorithms
- **occ_arr.ml** - module for managing table of occurencies
- **huff_tree.ml** - module for managing huffman tree 
- **huffman.ml** - module managing compressing, decompressing and stats logic
- **huff.ml** - main file of the app that manages CLI logic
- **main.ml** - main entry to test everything
- **test_files** - folder with different text files to test the program

## Usage
1. Build the program: `dune build`
2. Run tests: `./_build/default/unit_test.exe`
3. Compress file: `./_build/default/huff.exe <file name>.txt`
4. Decompress file: `./_build/default/huff.exe <file name>.hf`
5. See stats of compression:  `./_build/default/huff.exe --stats <file name>.txt`

There're some txt files in `test_files` directoty where you can test different files.

