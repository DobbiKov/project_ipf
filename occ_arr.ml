
(* utilities *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let is_singleton = function
    | h :: [] -> true
        | _ -> false

let is_empty = function
    | [] -> true
    | _ -> false

let find_min_in_sorted = function
    | h :: t -> h
    | _ -> failwith "the list is empty"

(* other part *)

let count_occs in_arr = 

    let rec count_for_one_byte arr l acc =
        match arr with
        | [] -> begin 
            match acc with
            | [] -> [(l, 1)]
            | h :: t -> (l, 1) :: acc
        end
        | h :: t -> (
            if (fst h) == l then
                 ((l, (snd h) + 1) :: acc) @ t
            else count_for_one_byte t l (h :: acc)
        )
    in

    let f acc x = 
        count_for_one_byte acc x []
    in
    List.fold_left f [] in_arr 

let sort_count_occs l =
    let comp el1 el2 =
        compare (snd el1) (snd el2)
    in
    List.sort comp l

let construct_occs_table in_arr = 
    in_arr |> count_occs |> sort_count_occs

let print_occ_list l =
    List.iter (fun x -> Printf.printf "(%c, %d)" ( (fst x) |> Char.chr ) (snd x)) l

