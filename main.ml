let file_name = "temp_file.txt"

let rec byte_to_string btt = 
    let rec aux bt num = 
        if num = 0 then ""
        else 
            if bt - num >= 0 then 
                "1" ^ (aux ( bt - num ) (num/2))
            else 
                "0" ^ (aux ( bt ) ( num/2 ))
    in
    aux btt 128

let bytes = file_name |> Read_file.read_file_in_bytes_for_compression
(*let () = List.iter (fun x -> Printf.printf "%c " ( x |> Char.chr )) bytes*)
(*let () = file_name |> Read_file.read_file_in_bytes_for_compression |> Occ_arr.construct_occs_table |> Occ_arr.print_occ_list*)
(*let () = print_endline ""*)
let huff_tree_a = file_name |> Read_file.read_file_in_bytes_for_compression |> Occ_arr.construct_occs_table |> Huff_tree.construct_huff_tree |> Huff_tree.tree_to_arr
let () = List.iter (fun x -> Printf.printf "%d - %s\n" (fst x) (( snd x ) |> byte_to_string)) huff_tree_a
let () = Write_file.write_compressed_file "temp_file.hf" huff_tree_a bytes
(*let () = List.iter (fun x -> Printf.printf "%d - %d\n" (fst x) (( snd x ))) huff_tree_a*)
