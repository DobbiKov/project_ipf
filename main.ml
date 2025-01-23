let file_name = "./temp_file.txt"

let rec byte_to_string btt = 
    let rec aux bt num = 
        if num = 0 then ""
        else 
            if bt - num >= 0 then 
                "1" ^ (aux ( bt - num ) (num/2))
            else 
                "0" ^ (aux ( bt ) ( num/2 ))
    in
    aux btt 32768 

let bytes = file_name |> Read_file.read_for_compression
let () = List.iter (fun x -> x |> Char.chr |> Printf.printf "%c") bytes
let () = print_endline ""

let contents = bytes |> List.map (fun x -> x |> Char.chr ) |> List.rev
let () = List.iter (Printf.printf "%c") contents
let () = print_endline ""

(*writing compressed file*)
let huff_arr = bytes |> Occ_arr.construct_occs_table |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr_2
let () = Write_file.write_compressed_file "temp_file.txt.hf" huff_arr bytes 

let tree, res = Read_file.read_for_decompression "temp_file.txt.hf" 
let decompressed_bytes = Huff_tree.compressed_bytes_to_bytes tree res
let () = List.iter (fun x -> x |> Char.chr |> Printf.printf "%c") decompressed_bytes 

(*let rec print_arr_of_bits arr = *)
(*    match arr with*)
(*    | [] -> ""*)
(*    | h :: t -> ( string_of_int h ) ^ (t |> print_arr_of_bits)*)
(**)
(*let huff_tree_a = file_name |> Read_file.read_for_compression |> Occ_arr.construct_occs_table |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr_2*)
(*let () = List.iter (fun x -> Printf.printf "%c - %s\n" (x |> fst |> Char.chr) (( snd x ) |> Huff_tree.bit_tab_to_str)) huff_tree_a*)
(**)

