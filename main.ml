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

let bytes = file_name |> Read_file.read_file_in_bytes_for_compression
(*let () = List.iter (fun x -> Printf.printf "%c " ( x |> Char.chr )) bytes*)
(*let () = file_name |> Read_file.read_file_in_bytes_for_compression |> Occ_arr.construct_occs_table |> Occ_arr.print_occ_list*)
(*let () = print_endline ""*)
let rec print_arr_of_bits arr = 
    match arr with
    | [] -> ""
    | h :: t -> ( string_of_int h ) ^ (t |> print_arr_of_bits)

let huff_tree_a = file_name |> Read_file.read_file_in_bytes_for_compression |> Occ_arr.construct_occs_table |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr_2
let () = List.iter (fun x -> Printf.printf "%c - %s\n" (x |> fst |> Char.chr) (( snd x ) |> Huff_tree.bit_tab_to_str)) huff_tree_a
let how_many = Write_file.write_compressed_file "temp_file.hf" huff_tree_a bytes
let _ = Printf.printf "\n Read bits: %d\n" (( bytes |> List.length ) * 8)
let _ = Printf.printf "\n written bits: %d\n" how_many
(*let () = List.iter (fun x -> Printf.printf "%d - %d\n" (fst x) (( snd x ))) huff_tree_a*)

(*let () = print_endline "\n"*)
(*let huff_tab, rest = "temp_file.txt.hf" |> Read_file.read_file_for_decompress *)
(*let () = Printf.printf "tab len: %d\n" (huff_tab |> List.length)*)
(*let () = List.iter (fun x -> Printf.printf "%c - %s\n" (x |> fst |> Char.chr) (( snd x ))) huff_tab *)
(*let () = List.iter (fun x -> Printf.printf "%s " x) ( rest |> List.rev )  *)
(*let () = List.iter (fun x -> Printf.printf "%c " ( ( Huff_tree.get_byte_in_huff_tree_tab huff_tab x ) |> Char.chr )) ( rest |> List.rev )  *)
