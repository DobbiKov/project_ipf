open Huff_tree

val write_compressed_file: string -> compressed_byte huff_tree_arr -> byte_arr -> unit
    (**
    [write_compressed_file fname huff_table file_bytes] takes file name, huff_table for compression and data to write
and writes in a file a compressed version of given data
    *)

val write_decompressed_file: string -> 'a huff_tree_arr -> 'a list -> unit
    (**
    [write_decompressed_file fname huff_table file_bytes] takes file name, huff_table for decompression and compressed data.
    writes decompressed version of the data into the file
    *)
