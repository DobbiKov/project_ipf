type 'a occ_table = ('a * int) list

(* utilities *)
let int_to_bits n =
  (* Converts an integer (0-255) to an array of 8 bits *)
  if n < 0 || n > 255 then
    invalid_arg "Input must be in the range 0-255"
  else
    let rec to_bits n count acc =
      if count = 0 then acc
      else
        let bit = n land 1 in
        to_bits (n lsr 1) (count - 1) (bit :: acc)
    in
    to_bits n 8 []

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
    (* returns an array of format [( byte(char), int(frequency) ), ...]*)
    let rec count_for_one_byte acc elem acc2 =
        (*counts a number of occurencies of the element [elem] in the [acc]*)
        match acc with
        | [] -> begin (*if acc is empty, then we iterated over all the elements and didn't find [elem]*)
            match acc2 with
            | [] -> [(elem, 1)] (*if acc2 is also empty, then we only start iterating the list using fold_left*)
            | h :: t -> (elem, 1) :: acc2 (*if acc2 is not empty, then we haven't analyzed [elem] yet, thus we add it to acc2 and return it*)
        end
        | h :: t -> ( (*acc not empty, then we are iterating list that looks like [(char, frequency)]*)
            if (fst h) = elem then (*if we find an element (char, frequence) such that char == elem, then we return add 1 to frequency (cause +1 elem) and return all the acc*)
                 ((elem, (snd h) + 1) :: acc2) @ t
            else count_for_one_byte t elem (h :: acc2) (*if we haven't found it yet, we iterate further the list and adding the element h to the temp acc*)
        )
    in

    let f acc x = 
        count_for_one_byte acc x []
    in
    List.fold_left f [] in_arr 


let construct_occs_table in_arr = 
    (*making table of frequencies and sorting it by frequency of each char*)
    let sort_count_occs l =
        let comp el1 el2 =
            compare (snd el1) (snd el2)
        in
        List.sort comp l
    in
    in_arr |> count_occs |> sort_count_occs

let print_occ_list l =
    List.iter (fun x -> Printf.printf "(%c, %d)" ( (fst x) |> Char.chr ) (snd x)) l
