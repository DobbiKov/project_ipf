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

let handle_stats fname =
    if check_file_exists fname = false then
        failwith "The file doesn't exists"
    else
        let len_before = Huffman.stats fname in
        Huffman.compress fname;
        let len_after = Huffman.stats ( fname ^ ".hf" ) in
        print_endline "Statistique de la compression:";
        Printf.printf "Taille d'un fichier %s (avant compression): %d bytes\n" fname len_before;
        Printf.printf "Taille d'un fichier %s (apres compression): %d bytes\n" ( fname ^ ".hf" ) len_after 

let main_handler args =
    if Array.length args < 2 then
        print_endline "Try: huff --help"
    else match args.(1) with
    | "--help" -> print_endline "help"
    | "--stats" -> 
            if args |> Array.length < 3 then 
                print_endline "Try: huff --help"
            else
                handle_stats args.(2) 
    | file_name -> handle_file_name file_name

let () = main_handler (Sys.argv)
