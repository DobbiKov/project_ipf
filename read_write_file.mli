open Huff_tree

val read_and_write_for_decompression: string -> string -> unit
    (**
    [read_for_decompression fname fname_out] takes a filename of compressed file and filename where to write decompressed file, 
    reads the first one, decompresses the date and writes it to the second file
    *)

val write_compressed_file: string -> compressed_byte huff_tree_arr -> string -> unit
    (**
    [write_compressed_file fname huff_table old_file_name] takes file name, huff_table for compression and old_file_name 
and writes in a file a compressed version of the data from given old file
    *)
