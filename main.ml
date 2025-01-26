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

let occ_table = "test_files/equal.txt" |> Occ_arr.count_occs 
let () = List.iter (fun (a, b) -> Printf.printf "%c %d\n" (a |> Char.chr) b) occ_table

let heap = occ_table |> Occ_arr.occ_table_to_heap

let rec print_heap h = 
    if Heap.is_empty h then ()
    else begin
        let min, rest = Heap.remove_min h in
        Printf.printf "h: %c %d\n" (min |> fst |> Char.chr) (min |> snd);
        print_heap rest
    end
let () = print_heap heap
    

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

