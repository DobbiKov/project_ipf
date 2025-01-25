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


let () =
    let occ_table = [  
    (97, 45);  (* 'a' *)
    (98, 13);  (* 'b' *)
    (99, 12);  (* 'c' *)
    (100, 16); (* 'd' *)
    (101, 9);  (* 'e' *)    
    (102, 5)   (* 'f' *)] in

    let huff_tree = Huff_tree.construct_huff_tree occ_table in 

    let huff_arr = Huff_tree.tree_to_arr_2 huff_tree in

    let processed_bits = Huff_tree.process_huff_tree_tab huff_arr in



    Printf.printf "Huffman Tree Array:\n";
    List.iter (fun (byte, bits) ->
        Printf.printf "Byte: %c, Bits: " (Char.chr byte);
        List.iter (Printf.printf "%d") bits;
        Printf.printf "\n"
    ) huff_arr;

    (* Print the processed bits *)
    Printf.printf "\nProcessed Bits:\n";
    Huff_tree.print_bit_tab processed_bits;
    Printf.printf "\nHuffman Tree Construction Test Successful.\n"

