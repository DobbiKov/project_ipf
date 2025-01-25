open Occ_arr
open Huff_tree

let decode_and_print_huffman_tree input_string =
  (* print function *)
    let txt_file = "temp_unit_test_file.txt" in
    let hf_file = "temp_unit_test_file.txt.hf" in

    let write_seq seq =
        let och = open_out txt_file in
        List.iter (fun x -> output_byte och x) seq;
        close_out och;
        ()
    in 

  let rec print_huffman_tree tree indent =
    match tree with
    | Nil -> Printf.printf "%sNil\n" indent
    | Leaf x -> Printf.printf "%sLeaf('%c')\n" indent (char_of_int x)
    | Node (left, right) ->
        Printf.printf "%s+--Node\n" indent;
        let new_indent = indent ^ "|  " in
        print_huffman_tree left new_indent;
        print_huffman_tree right new_indent
  in

  let input_bytes =
    input_string
    |> String.to_seq
    |> List.of_seq
    |> List.map Char.code
    |> List.rev
  in

  input_bytes |> List.rev |> write_seq;
  
  let freq_table = txt_file |> Occ_arr.count_occs in

  Printf.printf "=== Occurrence Table ===\n";
  print_occ_list freq_table;
  Printf.printf "\n\n";

  
  let huffman_tree = construct_huff_tree freq_table in

  Printf.printf "=== Constructed Huffman Tree ===\n";
  print_huffman_tree huffman_tree "";
  Printf.printf "\n";

  (* Convert the tree to a bitlist representation *)
  let arr2 = tree_to_arr_2 huffman_tree in

  Printf.printf "=== Tree to Array 2 (bit representations) ===\n";
  List.iter (fun (ch, bits) ->
    Printf.printf "('%c', " (Char.chr ch);
    print_bit_tab bits;
    Printf.printf ")  ";
  ) arr2;
  Printf.printf "\n\n";

  (* Compress the input string into bits *)
  let compressed_bits = bytes_to_compressed_bytes arr2 input_bytes in

  Printf.printf "=== Compressed Bit Stream ===\n";
  List.iter print_bit_tab compressed_bits;
  Printf.printf "\n\n";

  Printf.printf "=== Encoded Bits ===\n";
  Printf.printf "%s\n\n" (compressed_bits |> List.map bit_tab_to_str |> String.concat "");

  (* Decompress the bits back to bytes *)
  let decoded_bytes = compressed_bytes_to_bytes arr2 compressed_bits in

  (* Convert the decompressed bytes back to a string *)
  let decoded_string =
    decoded_bytes
    |> List.map Char.chr
    |> List.to_seq
    |> String.of_seq
  in

  Printf.printf "=== Decoded String ===\n";
  Printf.printf "%s\n\n" decoded_string;


  if decoded_string = input_string then
    Printf.printf "✅ Success\n"
  else
    Printf.printf "❌ Error\n";

    let new_txt_file = "temp_unit_test_file_decompressed.txt" in
    let huff_tab = txt_file |> Occ_arr.count_occs |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr_2 in
    Read_write_file.write_compressed_file hf_file huff_tab txt_file;
  Printf.printf "Compressed file written: %s\n" hf_file;

  Read_write_file.read_and_write_for_decompression hf_file new_txt_file;
  Printf.printf "Decompressed file written: %s\n" new_txt_file;

      let bef_ich = open_in txt_file in
      let aft_ich = open_in new_txt_file in
  let rec compare_before_after_compression () =
    try 
        let bef_byte = input_byte bef_ich in 
        try 
            let aft_byte = input_byte aft_ich in

            if bef_byte != aft_byte then begin false end
            else compare_before_after_compression ()
        with | _ -> Printf.printf "finished but why: %c" (bef_byte |> Char.chr); false
    with | _ -> begin
        try
            let _ = input_byte aft_ich in
            false
        with | _ -> true
    end
    in
    let is_same = compare_before_after_compression () in
    close_in bef_ich;
    close_in aft_ich;

    match is_same with
    | true -> Printf.printf "✅ Success: decompressed string matches input.\n\n";
    | false ->Printf.printf "❌ Error: decompressed string does NOT match input.\n\n";
    ()
(* Example usage: *)
let () =
  let test_strings = [
    "aaaaaa";               
    "abcabcabc";
    "a";                     
    " ";                     
    "hello world";            
    "The quick brown fox jumps over the lazy dog"; 
    "";                      
    "!@#$%^&*()";            
    "1234567890";            
    "你好世界";              
    "Привет мир";            
    "こんにちは世界";         
    String.init 10000 (fun _ -> 'a'); 
    "aaaabbbcc";             
    "abcdabcdabcdabcd";
    "ab";
  ] in

  List.iter (fun test_string ->
    Printf.printf "Testing string: %s\n" test_string;
    let _ = decode_and_print_huffman_tree test_string in
    Printf.printf "====================\n\n"
  ) test_strings

(*test heap*)
let occ_table = [  
    (97, 45);  (* 'a' *)
    (98, 13);  (* 'b' *)
    (99, 12);  (* 'c' *)
    (100, 16); (* 'd' *)
    (101, 9);  (* 'e' *)    
    (102, 5)   (* 'f' *)] 

let heap = Occ_arr.occ_table_to_heap occ_table

let rec print_heap h = 
    if Heap.is_empty h then ()
    else begin
        let min, rest = Heap.remove_min h in
        Printf.printf "%c | %d\n" (min |> fst |> Char.chr) (min |> snd);
        print_heap rest
    end

let () = print_heap heap


let () = 
  let h = Heap.empty in
  assert (Heap.is_empty h);
  assert (not (Heap.is_singleton h));

  let h = Heap.add ('a', 10) h in
  assert (Heap.find_min h = ('a', 10));
  assert (Heap.is_singleton h);

  let h = Heap.add ('b', 5) h in
  assert (Heap.find_min h = ('b', 5));  

  let h = Heap.add ('x', 20) h in
  assert (Heap.find_min h = ('b', 5));
  
  let h = Heap.add ('a', 1) h in
  assert (Heap.find_min h = ('a', 1));
  
  let h = Heap.add ('m', 7) h in
  assert (Heap.find_min h = ('a', 1));
  
  let ((l1, min1), h) = Heap.remove_min h in
  assert ((l1, min1) = ('a', 1));
  assert (Heap.find_min h = ('b', 5));
  
  let ((l2, min2), h) = Heap.remove_min h in
  assert ((l2, min2) = ('b', 5));
  assert (Heap.find_min h = ('m', 7));
  
  let ((l3, min3), h) = Heap.remove_min h in
  assert ((l3, min3) = ('m', 7));
  assert (Heap.find_min h = ('a', 10));
  
  let ((l4, min4), h) = Heap.remove_min h in
  assert ((l4, min4) = ('a', 10));
  assert (Heap.find_min h = ('x', 20));
  
  let ((l5, min5), h) = Heap.remove_min h in
  assert ((l5, min5) = ('x', 20));
  assert (Heap.is_empty h);
  
  Printf.printf "success\n"
  
