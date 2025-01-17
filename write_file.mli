
val write_compressed_file: string -> (int * int list) list -> int list -> unit
    (**
    [write_compressed_file fname huff_table file_bytes] takes file name, huff_table for compression and data to write
and writes in a file a compressed version of given data
    *)

val write_decompressed_file: string -> (int * 'a) list -> 'a list -> unit
    (**
    [write_decompressed_file fname huff_table file_bytes] takes file name, huff_table for decompression and compressed data.
    writes decompressed version of the data into the file
    *)
