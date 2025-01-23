open Bs

let write_compressed_file fname huff_table file_bytes =
  let och = open_out fname in
  let o_str = Bs.of_out_channel och in
  let tab_len = List.length huff_table in

  let rec write_bits_tab = function
      | [] -> ()
      | h :: t -> 

              Bs.write_bit o_str h; 
              write_bits_tab t
  in 

  tab_len |> Occ_arr.int_to_bits |> write_bits_tab;

  let huff_arr_to_write = Huff_tree.process_huff_tree_tab huff_table in   
  write_bits_tab huff_arr_to_write;
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
  (*let o_str = Bs.of_out_channel och in*)
  let decompressed_bytes = Huff_tree.compressed_bytes_to_bytes huff_table file_bytes in
  let rec write_comp_bytes = function
      | [] -> ()
      | h :: t -> 
              output_byte och h;
              write_comp_bytes t;
  in
  write_comp_bytes decompressed_bytes;
  (*Bs.finalize o_str;*)
  close_out och
