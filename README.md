# OCaml Project

## Authors
- Yehor KOROTENKO (yehor.korotenko@etu-upsaclay.fr)
- Ivan KHARKOV (ivan.kharkov@etu-upsaclay.fr)

## About
The project's goal is to write a CLI tool to compress and decompress files using the OCaml programming language that was learnt on **Introduction to Functional Programming** course taught by the professor **Kim Nguyễn** [link](https://usr.lmf.cnrs.fr/~kn/index_en.html)

## Setup
1. Make sure you have __ocaml__ and __dune__ installed on your machine
2. Clone the project `git clone https://github.com/DobbiKov/project_ipf`
3. Enter into the folder `cd project_ipf`
4. Execute the file, for example to see the help, write: `dune exec ./huff.exe --help`

## Temp docs
*read_file.ml* - module with functions to read a file and compressed file
*occ_arr.ml* - module for managing table of occurencies
*huff_tree.ml* - module for managing huffman tree 
*bs.ml* - module for reading and writing data to a file
*main.ml* - main entry to test everything
