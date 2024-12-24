let check_file_exists fname = 
    Sys.file_exists fname

let handle_compress_file fname = 
    if check_file_exists fname = false then
        failwith "The file doesn't exists"
    else
        Huffman.compress fname

let handle_decompress_file fname = 
    if check_file_exists fname = false then
        failwith "The file doesn't exists"
    else
        Huffman.decompress fname

let handle_file_name fname = 
    match Filename.check_suffix fname ".hf" with
    | true -> handle_decompress_file fname
    | false -> handle_compress_file fname

let main_handler args =
    if Array.length args < 2 then
        print_endline "Try: huff --help"
    else match args.(1) with
    | "--help" -> print_endline "help"
    | "--stats" -> print_endline "stats"
    | file_name -> handle_file_name file_name

let () = main_handler (Sys.argv)
