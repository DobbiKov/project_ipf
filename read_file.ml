
let read_file_in_bytes_for_compression fname = 
    let in_ch = open_in fname in
    let istr = Bs.of_in_channel in_ch in
    let rec loop i_str acc =
        try 
            let byte = ( Bs.read_byte i_str ) in
            (*Printf.printf "textim: %d | " (byte);*)
            Printf.printf "textim: %c\n" (( byte ) |> Char.chr );
            loop i_str (byte :: acc)
        with 
        | End_of_file -> 
                print_endline "end of file";
            close_in in_ch;
            acc
        | Bs.End_of_stream -> 
                print_endline "end of file";
            close_in in_ch;
            acc
        | ex ->  (* General catch-all for unexpected exceptions *)
            Printf.printf "Error: %s\n" (Printexc.to_string ex);
            close_in in_ch;
            acc
        | _ ->
                print_endline "other error";
            close_in in_ch;
            acc
    in
    (loop istr []) |> List.rev

let read_file_in_short_for_decompression fname = 
    let in_ch = open_in fname in
    let istr = Bs.of_in_channel in_ch in
    let rec loop i_str acc =
        try 
            let byte = ( Bs.read_n_bits i_str 32 ) in
            (*Printf.printf "textim: %d | " (byte);*)
            (*Printf.printf "textim: %c\n" (( byte ) |> Char.chr );*)
            loop i_str (byte :: acc)
        with 
        | _ -> 
            close_in in_ch;
            acc
    in
    (loop istr []) |> List.rev

let rec read_bits_compressed istr acc main_bit = 
        let bit = Bs.read_bit istr in
        match main_bit with
        | None -> read_bits_compressed istr (bit :: acc) (Some bit)
        | Some b -> begin
            if b = bit then
                read_bits_compressed istr (bit :: acc) (main_bit)
            else 
                (bit :: acc) |> List.rev
        end

let rec read_first_bits_compressed istr acc = 
    match ( read_bits_compressed istr acc (Some 1) ) |> List.rev with
    | [] -> [1]
    | h :: t -> t

let rec read_second_bits_compressed istr acc = 
    match ( read_bits_compressed istr acc (Some 0) ) |> List.rev with
    | [] -> [0]
    | h :: t -> t

let read_file_for_decompress fname = 
    let in_ch = open_in fname in
    let istr = Bs.of_in_channel in_ch in
    let tab_size = Bs.read_byte istr in

    let rec read_table_keys acc counter = 
        if counter = tab_size then acc
        else 
            let byte = Bs.read_byte istr in
            read_table_keys ( byte :: acc ) ( counter + 1 )
    in
    let temp_table_keys = read_table_keys [] 0 in

    let rec read_table_values acc counter = 
        if counter = tab_size then acc
        else if counter = 0 then begin
            let compr_bit = read_first_bits_compressed istr [] in
            read_table_values ( compr_bit :: acc ) ( counter + 1 )
        end
        else if counter = 1 then begin
            let compr_bit = read_second_bits_compressed istr [] in
            read_table_values ( compr_bit :: acc ) ( counter + 1 )
        end
        else 
            let compr_bit = read_bits_compressed istr [] None in
            read_table_values ( compr_bit :: acc ) ( counter + 1 )
    in
    let temp_table_values = read_table_values [] 0 in
    let rec merge_keys_values keys values acc =
        match keys, values with
        | [], arr1 -> acc |> List.rev
        | arr1, [] -> acc |> List.rev
        | hk :: tk, hv :: tv -> merge_keys_values tk tv ((hk, hv) :: acc)
    in
    let temp_table = merge_keys_values temp_table_keys temp_table_values [] in
    let table = temp_table |> Huff_tree.huff_tree_with_arr_to_huff_tree_with_str in  
    
    let () = List.iter (fun x -> Printf.printf "%c - %s\n" (x |> fst |> Char.chr) (x |> snd)) table in 
    let _ = Bs.read_bit istr in
    let rec read_tab_of_compr_bits acc_res acc_bit=
        try 
            let bit = Bs.read_bit istr in
            Printf.printf "%d" bit;
            let new_acc_bit = bit :: acc_bit in

            let bits_tab = new_acc_bit |> List.rev in
            let bits_str = Huff_tree.bit_tab_to_str bits_tab in
            let is_in_tab = Huff_tree.is_compr_byte_in_tree_tab bits_str table in
            match is_in_tab with
            | false -> read_tab_of_compr_bits acc_res new_acc_bit
            | true -> 
                    (*Printf.printf "(%s); " bits_str;*)
                    read_tab_of_compr_bits (bits_str :: acc_res) []

        with 
        | _ -> acc_res |> List.rev
    in
    print_endline "exatcly our data:";
    let res = read_tab_of_compr_bits [] [] in
    close_in in_ch;
    ( table, res )
    
