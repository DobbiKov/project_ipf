let decompress file_name = 
    let huff_tab, rest = file_name |> Read_file.read_for_decompression in
    (*get filename (without .hf) *)
    let new_fname = String.sub file_name 0 ( (String.length file_name) - 3 ) in
    Write_file.write_decompressed_file new_fname huff_tab rest

let compress file_name = 
    let bytes = file_name |> Read_file.read_for_compression in
    let huff_tree_a = bytes |> Occ_arr.construct_occs_table |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr_2 in
    Write_file.write_compressed_file ( file_name ^ ".hf" ) huff_tree_a bytes
    

let input_code i_ch = 
    try
        input_byte i_ch
    with e -> -1

let stats file_name = 
    let i_ch = open_in file_name in
    let rec loop count = 
        match input_code i_ch with
        | bt when bt < 0 -> count
        | _ -> loop (count + 1)
    in
    loop 0
