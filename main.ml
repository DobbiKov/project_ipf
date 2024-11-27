let text_var = "satisfaisant"

type tree = 
    Leaf of char
    | Node of tree * tree
    | Nil

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

let () = print_endline ""
let () = print_tree 0 ( res |> construct_huff_tree ) 

(* | h :: t -> begin 

end *)


