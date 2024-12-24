open Occ_arr

type tree = 
    Leaf of int 
    | Node of tree * tree
    | Nil

let construct_huff_tree l =

    let rec construct left right acc = 
        match left, right with
        | (_, Nil), _ -> begin
            if is_empty acc then 
               Nil 
            else if is_singleton acc then
                Node (Leaf (acc |> find_min_in_sorted |> fst),  Nil)
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
                Node (snd left,  Leaf (acc |> find_min_in_sorted |> fst))
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
    let rec loop tr' integer count =
        match tr' with
        | Nil -> []
        | Leaf x -> [ (x, integer * (pow 2 (8 - count))) ]
        | Node (x, y) -> 
                (loop x (integer * 2) (count + 1)) @ (loop y ((integer * 2) + 1) (count + 1))
    in
    let temp_res = loop tr 0 0 in

    let comp el1 el2 =
        compare (el1 |> snd) (el2 |> snd)
    in

    ( List.sort comp temp_res ) |> List.rev
