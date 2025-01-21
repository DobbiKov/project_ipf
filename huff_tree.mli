type tree = 
    Leaf of int 
    | Node of tree * tree
    | Nil

val construct_huff_tree: (int * int) list -> tree
    (**
    [construct_huff_tree l] takes an array of occurencies and returns back a huff tree 
    *)
val tree_to_arr_2: tree -> (int * int list) list
    (**
    [tree_to_arr_2 tr] takes a huff tree and returns and rerturns an array in format [(key, value), ...] 
    where key represents byte (i.e char) and value represents converted version of the byte (i.e in 11110, 00001 format)
    The bytes that are in compressed version are represented as 111111 and 000000 are placed in the start of the array.
        *)
val process_huff_tree_tab: (int * int list) list -> int list
    (**
    [process_huff_tree_tab huff_tree]
    takes an array of huff tree and converts it to the list where the keys are first, 
    then values in the second. Each key and value is converted to bit format. The result is an array of bits
    *)

val is_compr_byte_in_tree_tab: 'a -> ('b * 'a) list -> bool
(** 
    [is_compr_byte_in_tree_tab key huff_tree]
    verifies if compressed byte (in format 11110, 00001) is in the table
return true if it is
and false in the other case
 *)

val get_compressed_byte_in_huff_tree_tab: ('a * 'b) list -> 'a -> 'b
(**
[get_compressed_byte_in_huff_tree_tab huff_tree_tab key] takes an array of huff tree and a byte that represents a char and returns it's compressed form from the huff tree
 *)

val get_byte_in_huff_tree_tab: (int * 'a) list -> 'a -> int
(**
[get_byte_in_huff_tree_tab huff_tree_tab value] takes an array of huff_tree and compressed byte (i.e in 11110 00001 format) and gives an uncompressed byte (i.e char)
 *)

val bytes_to_compressed_bytes: (int * int list) list -> int list -> int list list
(**
    [bytes_to_compressed_bytes huff_tree_tab bytes_tab] takes array of huff_tree and array of bytes (chars) and returns same array with compressed bytes (i.e in 1110, 0001 form)
     *)

val compressed_bytes_to_bytes: (int * 'a) list -> 'a list -> int list
(**
    [compressed_bytes_to_bytes huff_tree_tab bytes_tab] takes an array of huff_tree and an array of compressed bytes (i.e if 11110, 00001 form) 
    and returns an array of these bytes but in decompressed version (i.e chars)
     *)

val bit_tab_to_str: int list -> string
    (**
    [bit_tab_to_str tab] takes a compressed byte in the form of bit array and returns this bit array in the form of string
    *)

val huff_tree_with_arr_to_huff_tree_with_str: ('a * int list) list -> ('a * string) list
    (**
    [huff_tree_with_arr_to_huff_tree_with_str tab] takes an array of huff_tree where compressed bytes are represented in the form of bit array 
    and returns the same huff_tree where the compressed bits are converted to strings
    *)
val print_bit_tab: int list -> unit
(**
   [print_bit_tab arr] prints a list of ints (supposed ones and zeros that represent a sequence of bits)
 *)
