let int_to_bits n =
  (* Converts an integer (0-255) to an array of 8 bits *)
  if n < 0 || n > 255 then
    invalid_arg "Input must be in the range 0-255"
  else
    let rec to_bits n count acc =
      if count = 0 then acc
      else
        let bit = n land 1 in
        to_bits (n lsr 1) (count - 1) (bit :: acc)
    in
    to_bits n 8 []
     
let write_compressed_file fname huff_table file_bytes =
    let och = open_out fname in
    let o_str = Bs.of_out_channel och in
    let tab_len = List.length huff_table in
    (*Bs.write_byte o_str tab_len; *)

    let rec write_bits_tab = function
        | [] -> ()
        | h :: t -> 
                (*temp code*) Printf.printf "%d" h;
                Bs.write_bit o_str h; write_bits_tab t
    in 

    tab_len |> int_to_bits |> write_bits_tab;

    let huff_arr_to_write = Huff_tree.process_huff_tree_tab huff_table in   
    write_bits_tab huff_arr_to_write;
    (*print_endline "writing keys";*)
    (*write_tab_keys_aux huff_table;*)
    (*print_endline "end writing keys";*)
    (*print_endline "writing values";*)
    (*write_tab_values_aux 0 huff_table;*)
    (*print_endline "end writing values";*)
    (**)
    (*print_endline "bytes to compressed start";*)
    let compressed_bytes = Huff_tree.bytes_to_compressed_bytes huff_table file_bytes in
    (*print_endline "bytes to compressed end";*)
    let rec write_comp_bytes = function
        | [] -> ()
        | h :: t -> write_bits_tab h; print_endline ""; write_comp_bytes t;
    in
    print_endline "exactly our data:";
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
