let write_compressed_file fname huff_table file_bytes =
    let och = open_out fname in
    let o_str = Bs.of_out_channel och in
    let tab_len = List.length huff_table in
    Bs.write_byte o_str tab_len; 

    let rec write_bits_tab = function
        | [] -> ()
        | h :: t -> Bs.write_bit o_str h; write_bits_tab t
    in 

    let rec write_tab_keys_aux = function
        | [] -> ()
        | h :: t -> 
                Bs.write_byte o_str (fst h);
                write_tab_keys_aux t;
    in
    let rec write_tab_values_aux counter = function
        | [] -> ()
        | h :: t -> 
                h |> snd |> write_bits_tab;
                if counter = 0 then Bs.write_bit o_str 0;
                if counter = 1 then Bs.write_bit o_str 1;
                write_tab_values_aux (counter+1) t;
                if t |> List.is_empty then 
                    let last_bit = match h |> snd |> List.rev with
                        | [] -> failwith "how is it possible write_tab_values_aux" 
                        | h :: t -> h
                    in
                    if last_bit = 0 then Bs.write_bit o_str 1
                    else Bs.write_bit o_str 0
    in
    
    write_tab_keys_aux huff_table;
    write_tab_values_aux 0 huff_table;
    let compressed_bytes = Huff_tree.bytes_to_compressed_bytes huff_table file_bytes in
    let rec write_comp_bytes = function
        | [] -> ()
        | h :: t -> write_bits_tab h; write_comp_bytes t;
    in
    write_comp_bytes compressed_bytes;
    Bs.finalize o_str;
    close_out och

let write_decompressed_file fname huff_table file_bytes = 
    let och = open_out fname in
    let o_str = Bs.of_out_channel och in

    let () = List.iter (fun x -> Printf.printf "%s " x) file_bytes in
    let decompressed_bytes = Huff_tree.compressed_bytes_to_bytes huff_table file_bytes in
    let rec write_comp_bytes = function
        | [] -> ()
        | h :: t -> Bs.write_byte o_str h; write_comp_bytes t;
    in
    write_comp_bytes decompressed_bytes;
    (*Bs.finalize o_str;*)
    close_out och
