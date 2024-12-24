let write_compressed_file fname huff_table file_bytes =
    let och = open_out fname in
    let o_str = Bs.of_out_channel och in
    let tab_len = List.length huff_table in
    Bs.write_n_bits o_str 32 tab_len;

    let rec write_tab_aux = function
        | [] -> ()
        | h :: t -> 
                Bs.write_n_bits o_str 32 (fst h);
                Bs.write_n_bits o_str 32 (snd h);
                write_tab_aux t;
    in
    
    write_tab_aux huff_table;
    let compressed_bytes = Huff_tree.bytes_to_compressed_bytes huff_table file_bytes in
    let rec write_comp_bytes = function
        | [] -> ()
        | h :: t -> Bs.write_n_bits o_str 32 h; write_comp_bytes t;
    in
    write_comp_bytes compressed_bytes;
    Bs.finalize o_str;
    close_out och

let write_decompressed_file fname huff_table file_bytes = 
    let och = open_out fname in
    let o_str = Bs.of_out_channel och in

    let decompressed_bytes = Huff_tree.compressed_bytes_to_bytes huff_table file_bytes in
    let rec write_comp_bytes = function
        | [] -> ()
        | h :: t -> Bs.write_byte o_str h; write_comp_bytes t;
    in
    write_comp_bytes decompressed_bytes;
    Bs.finalize o_str;
    close_out och
