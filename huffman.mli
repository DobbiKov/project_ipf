val decompress: string -> unit  
    (**
    [decompress file_name] takes a file name and decompresses the file into new one
    *)
val compress: string -> unit 
    (**
    [compress file_name] takes a file name and compresses the file into new one
    *)

val stats: string -> int 
(**
   [stats file_name] takes a filename and returns the number of bytes inside
 *)
