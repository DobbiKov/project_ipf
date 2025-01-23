open Occ_arr
open Huff_tree
open Write_file
open Read_file

let decode_and_print_huffman_tree input_string =
  (* print function *)
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

  
  let freq_table = construct_occs_table input_bytes in

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

  let compressed_file_name = "test.hf" in
  write_compressed_file compressed_file_name arr2 input_bytes;
  Printf.printf "Compressed file written: %s\n" compressed_file_name;

  let compressed_file_bytes = read_for_compression compressed_file_name in
  if  compressed_file_bytes = input_bytes then
    Printf.printf "✅ Success : verified compressed file bytes\n"
  else
    Printf.printf "❌ Error : compressed file bytes do not match input bytes\n";

  let decompressed_file_name = "test_decompressed.txt" in
  write_decompressed_file decompressed_file_name arr2 compressed_bits;
  Printf.printf "Decompressed file written: %s\n" decompressed_file_name;

  let (read_huff_table, final_compressed_bits) = read_for_decompression compressed_file_name in
  let decompressed_bytes = compressed_bytes_to_bytes read_huff_table final_compressed_bits in
  let decompressed_string = 
    decompressed_bytes 
    |> List.map Char.chr 
    |> List.to_seq 
    |> String.of_seq 
  in
  List.iter (fun (x, y) -> Printf.printf "%c %s\n" (Char.chr x) y) read_huff_table;

  if decompressed_string = input_string then
    Printf.printf "✅ Success: decompressed string matches input.\n\n"
  else
    Printf.printf "❌ Error: decompressed string does NOT match input.\n%s_%s\n" input_string decompressed_string;
  
  final_compressed_bits |> List.iter (Printf.printf "%s");
  ()
  


(* Example usage: *)
let () =
  let test_strings = [
    "aaaaaa";                 
    "ababababab";
    "abcabcabc";
    "a";                     
    " ";                     
    "hello world";            
    "The quick brown fox jumps over the lazy dog"; 
    (*"";                      
    "!@#$%^&*()";            
    "1234567890";            
    "你好世界";              
    "Привет мир";            
    "こんにちは世界";         
    String.init 10000 (fun _ -> 'a'); 
    "aaaabbbcc";             
    "abcdabcdabcdabcd"*)
  ] in

  List.iter (fun test_string ->
    Printf.printf "Testing string: %s\n" test_string;
    let _ = decode_and_print_huffman_tree test_string in
    Printf.printf "====================\n\n"
  ) test_strings
