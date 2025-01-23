
val read_for_compression: string -> int list 
    (**
        [read_for_compression fname] takes the filename, reads the file and returns an array of bytes(representing chars)
     *)

val read_for_decompression: string -> (int * string) list * string list
    (**
    [read_for_decompression fname] takes a filename, reads it and returns a huff_table for decompression and the rest of the file
    in the format of compressed data i.e an array of compressed bytes
    *)
