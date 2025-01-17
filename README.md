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
4. Write `dune build` to compile the project
5. Run `./_build/default/huff.exe`

## Temp docs
*bs.ml* - module for reading and writing data to a file
*read_file.ml* - module with functions to read files
*write_file.ml* - module with function to write files
*occ_arr.ml* - module for managing table of occurencies
*huff_tree.ml* - module for managing huffman tree 
*huffman.ml* - module managing compressing, decompressing and stats logic
*huff.ml* - main file of the app that manages CLI logic
*main.ml* - main entry to test everything
