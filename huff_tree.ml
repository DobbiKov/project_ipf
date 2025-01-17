open Occ_arr

type tree = 
    Leaf of int 
    | Node of tree * tree
    | Nil


let construct_huff_tree l =

    let rec construct left right acc = 
        match left, right with
        | (_, Nil), _ -> begin (*if left subtree is not initialized yet*)
            if Occ_arr.is_empty acc then 
               Nil 
            else if Occ_arr.is_singleton acc then
                Node (Leaf (acc |> Occ_arr.find_min_in_sorted |> fst),  Nil)
            else begin 
                match acc with
                | h1 :: h2 :: t -> (
                    let new_left_node = Node (Leaf (fst h1), Leaf (fst h2)) in
                    let new_left = ((snd h1) + (snd h2), new_left_node) in
                    construct new_left right t
                ) 
                | _ -> failwith "case impossible"
            end
                
        end
        | _, (_, Nil) -> begin (*if right subtree is not initialized yet*)
            if Occ_arr.is_empty acc then 
               Node (snd left, Nil) 
            else if Occ_arr.is_singleton acc then
                Node (snd left,  Leaf (acc |> Occ_arr.find_min_in_sorted |> fst))
            else begin 
                match acc with
                | h1 :: h2 :: t -> (
                    let new_right_node = Node (Leaf (fst h1), Leaf (fst h2)) in
                    let new_right = ((snd h1) + (snd h2), new_right_node) in
                    construct left new_right t
                ) 
                | _ -> failwith "case impossible"
            end
        end
        | _, _ -> begin (*if both subtrees are initialized*)
            match acc with
            | [] -> Node (snd left, snd right)
            | h :: t -> 
                if (fst left) <= (fst right) then (*if left is less frequents the the right one, we will place new node to the left*)
                    let new_left_node = Node (snd left, Leaf (fst h)) in
                    let new_left = ((fst left) + (snd h), new_left_node) in
                    construct new_left right t
                else (*in the other case to the right*)
                    let new_right_node = Node (Leaf (fst h), snd right) in
                    let new_right = ((fst right) + (snd h), new_right_node) in
                    construct left new_right t
        end
    in

    construct (0, Nil) (0, Nil) l

let tree_to_arr tr = 
    let rec loop tr' integer count =
        match tr' with
        | Nil -> []
        | Leaf x -> [ (x, integer * (pow 2 (32 - count))) ]
        | Node (x, y) -> 
                (loop x (integer * 2) (count + 1)) @ (loop y ((integer * 2) + 1) (count + 1))
    in
    let temp_res = loop tr 0 0 in

    let comp el1 el2 =
        compare (el1 |> snd) (el2 |> snd)
    in

    ( List.sort comp temp_res ) |> List.rev

let tree_to_arr_2 tr = 
    (*takes huff tree and returns an array of (key, value) where key is byte (char) and value is in bit format (11110, 00001)*)
    let rec loop tr' arr count =
        match tr' with
        | Nil -> []
        | Leaf x -> [ (x, arr |> List.rev) ]
        | Node (x, y) -> 
                (loop x (0 :: arr) (count + 1)) @ (loop y (1 :: arr) (count + 1))
    in
    let temp_res = loop tr [] 0 in

    let comp el1 el2 =
        compare (el1 |> snd) (el2 |> snd)
    in

    let res = ( List.sort comp temp_res ) in
    let last, without_last  = match res with
    | [] -> ((0, []), [])
    | h :: t -> (h, t |> List.rev) 
    in

    let first, without_first = match without_last with
    | [] -> ((0, []), [])
    | h :: t -> (h, t) 
    in

    first  :: last :: without_first


let process_huff_tree_tab huff_tree = 
    (* takes an array of huff tree and converts it to the list where the keys are first, then values in the second. Each key and value is converted to bit format. The result is an array of bits *)
    let rec sep_first_elem h_t acc1 acc2 = 
        match h_t with
        | [] -> [], [], []
        | h :: t ->
                t, (acc1 @ (h |> fst |> Occ_arr.int_to_bits)), (acc2 @ (h |> snd))
    in
    let rec separate_lists h_t acc1 acc2 = 
        match h_t with
        | [] -> acc1, acc2
        | h :: t ->
                separate_lists t (acc1 @ (h |> fst |> Occ_arr.int_to_bits)) (acc2 @ (h |> snd))
    in
(*here we add 0 in the end of first value, and 1 in the end of the second value, so we obtain 1111110 00000001*)
(* we need it so we could read properly our table during the decompression *)

    let h_t_1, f_vals, f_comp_bits = sep_first_elem huff_tree [] [] in
    let h_t_2, s_vals, s_comp_bits = sep_first_elem h_t_1 f_vals (f_comp_bits @ [0]) in
    let vals, comp_bits = separate_lists h_t_2 s_vals (s_comp_bits @ [1]) in

    let last_bit = match ( comp_bits |> List.rev ) with [] -> 0 | h :: t -> if h = 0 then 1 else 0 in
    vals @ ( comp_bits @ [last_bit] )

let rec is_compr_byte_in_tree_tab bits_str huff_tab = 
    match huff_tab with
    | [] -> false 
    | h :: t -> 
            if (snd h) = bits_str then true 
            else is_compr_byte_in_tree_tab bits_str t 

let rec get_compressed_byte_in_huff_tree_tab huff_tree_tab key =
    match huff_tree_tab with
    | [] -> raise ( Invalid_argument "the byte isn't found in the tab" )
    | h :: t -> 
            if (fst h) = key then snd h
            else get_compressed_byte_in_huff_tree_tab t key

let rec get_byte_in_huff_tree_tab huff_tree_tab value =
    match huff_tree_tab with
    (*| [] -> raise ( Invalid_argument "the compressed byte isn't found in the tab" )*)
    | [] -> 0 
    | h :: t -> 
            if (snd h) = value then fst h
            else get_byte_in_huff_tree_tab t value

let rec print_bit_tab = function 
    | [] -> ()
    | h :: t -> Printf.printf "%d" h; print_bit_tab t

let bytes_to_compressed_bytes huff_tree_tab bytes_tab = 
    let rec loop acc bt = 
        match bt with
        | [] -> acc
        | h :: t -> 
                let comp_byte = get_compressed_byte_in_huff_tree_tab huff_tree_tab h in 

                loop (comp_byte :: acc) t 
    in
    loop [] bytes_tab

let compressed_bytes_to_bytes huff_tree_tab bytes_tab = 
    let rec loop acc bt = 
        match bt with
        | [] -> acc
        | h :: t -> 
                let byte = get_byte_in_huff_tree_tab huff_tree_tab h in 
                loop (byte :: acc) t 
    in
    ( loop [] bytes_tab ) |> List.rev

let rec bit_tab_to_str tab =
    match tab with
    | [] -> ""
    | h :: t -> (h |> string_of_int) ^ (t |> bit_tab_to_str)

let huff_tree_with_arr_to_huff_tree_with_str tab =
    let rec aux acc ttab =
        match ttab with
        | [] -> acc
        | h :: t -> 
                let byte = h |> fst in
                let str =  h |> snd |> bit_tab_to_str in
                aux ((byte, str) :: acc) t
    in
    aux [] tab
