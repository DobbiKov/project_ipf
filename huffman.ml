let decompress file_name = 
    (*get filename (without .hf) *)
    let new_fname = String.sub file_name 0 ( (String.length file_name) - 3 ) in
    Read_write_file.read_and_write_for_decompression file_name new_fname
let compress file_name = 
    let huff_tree_a = file_name |> Occ_arr.count_occs |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr_2 in
    Read_write_file.write_compressed_file ( file_name ^ ".hf" ) huff_tree_a file_name
    

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
