let decompress file_name = 
    let bytes = file_name |> Read_file.read_file_in_bytes_for_compression in
    let huff_tree_a, rest = Huff_tree.compressed_file_bytes_to_huff_tree_arr bytes in
    let new_fname = String.sub file_name 0 ( (String.length file_name) - 3 ) in
    Write_file.write_decompressed_file new_fname huff_tree_a rest

let compress file_name = 
    let bytes = file_name |> Read_file.read_file_in_bytes_for_compression in
    let huff_tree_a = file_name |> Read_file.read_file_in_bytes_for_compression |> Occ_arr.construct_occs_table |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr in
    Write_file.write_compressed_file ( file_name ^ ".hf" ) huff_tree_a bytes
