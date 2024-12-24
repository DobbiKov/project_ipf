let text_var = "test text hey\n"

type tree = 
    Leaf of char
    | Node of tree * tree
    | Nil

let explode s = List.init (String.length s) (String.get s)

let count_occs text = 

    let rec count_for_one_letter arr l acc =
        match arr with
        | [] -> begin 
            match acc with
            | [] -> [(l, 1)]
            | h :: t -> (l, 1) :: acc
        end
        | h :: t -> (
            if (fst h) == l then
                 ((l, (snd h) + 1) :: acc) @ t
            else count_for_one_letter t l (h :: acc)
        )
    in

    let f acc x = 
        count_for_one_letter acc x []
    in
    String.fold_left f [] text

let sort_count_occs l =
    let comp el1 el2 =
        compare (snd el1) (snd el2)
    in
    List.sort comp l

let construct_count_occs text = 
    text |> count_occs |> sort_count_occs

let print_occ_list l =
    List.iter (fun x -> Printf.printf "(%c, %d)" (fst x) (snd x)) l

let is_singleton = function
    | h :: [] -> true
        | _ -> false

let is_empty = function
    | [] -> true
    | _ -> false

let find_min = function
    | h :: t -> h
    | _ -> failwith "the list is empty"

let construct_huff_tree l =

    let rec construct left right acc = 
        match left, right with
        | (_, Nil), _ -> begin
            if is_empty acc then 
               Nil 
            else if is_singleton acc then
                Node (Leaf (acc |> find_min |> fst),  Nil)
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
        | _, (_, Nil) -> begin
            if is_empty acc then 
               Node (snd left, Nil) 
            else if is_singleton acc then
                Node (snd left,  Leaf (acc |> find_min |> fst))
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
        | _, _ -> begin
            match acc with
            | [] -> Node (snd left, snd right)
            | h :: t -> 
                if (fst left) <= (fst right) then 
                    let new_left_node = Node (snd left, Leaf (fst h)) in
                    let new_left = ((fst left) + (snd h), new_left_node) in
                    construct new_left right t
                else 
                    let new_right_node = Node (Leaf (fst h), snd right) in
                    let new_right = ((fst right) + (snd h), new_right_node) in
                    construct left new_right t
        end
    in

    construct (0, Nil) (0, Nil) l

let tree_to_arr tr = 
    let rec loop tr' str =
        match tr' with
        | Nil -> []
        | Leaf x -> [ (x, str) ]
        | Node (x, y) -> 
                (loop x (str ^ "0")) @ (loop y (str ^ "1"))
    in
    let temp_res = loop tr "" in

    let comp el1 el2 =
        compare (el1 |> snd |> String.length) (el2 |> snd |> String.length)
    in

    List.sort comp temp_res

let rec char_to_byte c table =
    match table with 
    | [] -> failwith "haven't found any c occurences"
    | h :: t when (fst h) = c -> snd h
    | h :: t -> char_to_byte c t

let build_compression_table text = 
    let occs = text |> count_occs |> sort_count_occs in
    let tree = occs |> construct_huff_tree in
    let comp_table = tree |> tree_to_arr in
    comp_table

let () = print_endline "hello, test:"
let () = List.iter (fun x -> Printf.printf "%d - %s\n" ( (fst x) |> Char.code ) (snd x)) (text_var |> build_compression_table) 

let compress_text text = 
    let comp_table = build_compression_table text in
    let rec table_to_str = function
        | [] -> ""
        | h :: t -> (snd h) ^ " " ^ (h |> fst |> Char.code |> string_of_int) ^ " " ^ (t |> table_to_str)
    in
    let str_table = comp_table |> table_to_str in

    let f acc x = 
        acc ^ (char_to_byte x comp_table)
    in
    let table_len_str = comp_table |> List.length |> string_of_int in
    let compressed_text = ( String.fold_left f "" text ) in
    table_len_str ^ " " ^ str_table ^ compressed_text 

let rec find_byte_to_char str table =
    match table with 
    | [] -> (false, ' ')
    | h :: t when str = snd h -> (true, fst h)
    | h :: t -> find_byte_to_char str t

let decompress_text compressed_text = 
    let list_of_chars = explode compressed_text in

    let rec read_til_space l =
        match l with
        | [] -> ("", [])
        | h :: t when h = ' ' -> ("", t)
        | h :: t -> begin
            let temp_res = read_til_space t in
            ((Char.escaped h) ^ (fst temp_res), snd temp_res)
        end
    in

    let (tab_len_str, rest_of_text) = read_til_space list_of_chars in
    let tab_len = tab_len_str |> int_of_string in

    let rec elems_for_table acc l len =
        if len = 0 then (acc, l)
        else 
            let (key, res_l) = read_til_space l in
            let (value, res_res_l) = read_til_space res_l in
            elems_for_table ((key, value) :: acc) res_res_l (len - 1)
        
    in

    let (list_of_elems_for_table, rest_of_text2) = elems_for_table [] rest_of_text tab_len in

    let rec construct_table = function
        | [] -> [] 
        | h :: t -> (h |> snd |> int_of_string |> Char.chr, h |> fst) :: construct_table  t
    in

    let table = construct_table list_of_elems_for_table in

    let rec bytes_to_text curr rest tab = 
        match rest with 
        | [] -> begin
            let (is_found, found_symb) = find_byte_to_char curr tab in
            if not is_found then ""
            else found_symb |> Char.escaped 
        end
        | h :: t -> begin 
            let (is_found, found_symb) = find_byte_to_char curr tab in
            if is_found then (found_symb |> Char.escaped) ^ (bytes_to_text (h |> Char.escaped) t tab)
            else bytes_to_text (curr ^ (h |> Char.escaped)) t tab
        end
    in
    bytes_to_text "" rest_of_text2 table




let res = text_var |> count_occs |> sort_count_occs
let () = print_occ_list res

let rec print_tree count = function
    | Nil -> ()
    | Leaf x -> Printf.printf "%c\n" x
    | Node (x, y) -> 
            Printf.printf "left:%d(\n" count;
            print_tree (count + 1) x;
            Printf.printf ")%d\n" count;
            Printf.printf "right:%d(\n" count;
            print_tree (count + 1) y;
            Printf.printf ")%d\n" count;
            ()

(* let () = print_endline "" *)
(* let () = print_tree 0 ( res |> construct_huff_tree )  *)
let () = print_endline ""

let rec print_tree_arr = function
    | [] -> ()
    | h :: t -> Printf.printf "(symbol: %c, code: %s), " (fst h) (snd h); print_tree_arr t; ()

let () = res |> construct_huff_tree |> tree_to_arr |> print_tree_arr

let comps_text = compress_text text_var
let () = Printf.printf "\n%s\n" comps_text
let new_table = decompress_text comps_text 
let () = Printf.printf "%s\n" new_table
(*let () = List.iter (fun x -> Printf.printf "%s %s " (fst x) (snd x)) new_table*)
(*let () = print_tree_arr new_table *)

(* | h :: t -> begin 

end *)


