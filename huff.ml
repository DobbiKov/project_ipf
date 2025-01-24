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

let calc_effic taille_bef taille_aft =
    let new_taille = taille_bef - taille_aft in
    let eff = (new_taille |> float_of_int) /. (taille_bef |> float_of_int) in
    eff

let handle_stats fname =
    if check_file_exists fname = false then
        failwith "The file doesn't exists"
    else
        let len_before = Huffman.stats fname in
        Huffman.compress fname;
        let len_after = Huffman.stats ( fname ^ ".hf" ) in
        print_endline "Statistique de la compression:";
        Printf.printf "Taille d'un fichier %s (avant compression): %d bytes\n" fname len_before;
        Printf.printf "Taille d'un fichier %s (apres compression): %d bytes\n" ( fname ^ ".hf" ) len_after;

        match calc_effic len_before len_after with
        | n when n <= 0. -> Printf.printf "Éspace perdu: %d%%\n" ( (n *. 100.) |> int_of_float )
        | n -> Printf.printf "Éspace économisé: %d%%\n" (( n *. 100.) |> int_of_float )

let help_handler () =
    print_endline "man de huff cli:
— huff --help : affiche ce message d’aide sur les différentes options
— huff <fichier> : compresse le fichier donné en argument pour produire un fichier fichier.hf
— huff <fichier.hf> : décompresser le fichier donné en argument pour produire un fichier fichier
— huff --stats <fichier> : compresse le fichier et affiche aussi des statistiques sur ce dernier"

let main_handler args =
    if Array.length args < 2 then
        print_endline "Try: huff --help"
    else match args.(1) with
    | "--help" -> help_handler ()
    | "--stats" -> 
            if args |> Array.length < 3 then 
                print_endline "Try: huff --help"
            else
                handle_stats args.(2) 
    | file_name -> handle_file_name file_name

let () = main_handler (Sys.argv)
