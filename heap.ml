type ('a, 'b) t = { size: int; elements: ('a * 'b) list }

let empty = { size = 0; elements = [] }

let is_empty h = (h.size = 0)

let is_singleton h = (h.size = 1)


let min_heapify h =
  let rec heapify i h =
    let left = 2 * i + 1 in
    let right = 2 * i + 2 in   
    
    (* Find the smallest among the current node, left child, right child *)
    let smallest =
      match List.nth_opt h.elements left with
      | Some v when (v |> snd) < (i |> List.nth h.elements |> snd) -> left
      | _ -> i
    in
    
    let smallest =
      match List.nth_opt h.elements right with
      | Some v when (v |> snd) < (smallest |> List.nth h.elements |> snd)  -> right
      | _ -> smallest
    in
    
    (* if current element is not the smallest, peform a swap, and continue heapify *)
    if smallest <> i then
      let swapped = List.mapi (fun j x ->
        if j = i then List.nth h.elements smallest       
        else if j = smallest then List.nth h.elements i 
        else x                                           
      ) h.elements in
      heapify smallest { h with elements = swapped }
    else
      h  
  in
  if h.size = 0 then h  
  else
    let rec heapify_all i h =
      if i < 0 then h
      else
        let h = heapify i h in
        heapify_all (i - 1) h
    in
    heapify_all ((h.size / 2) - 1) h

let add e h =
  let new_elements = e :: h.elements in
  let new_size = h.size + 1 in
  let reversed_elements = List.rev new_elements in
  let new_heap = { size = new_size; elements = reversed_elements } in
  min_heapify new_heap


let find_min = function
  | {size = 0; _ } -> failwith "Heap is empty"
  | {size; elements = [] } -> failwith "Heap size != 0, but empty"
  | {size = 1; elements = [x] } -> x  
  | { elements = x :: _; _ } -> x                             


let remove_min = function
  | { size = 0; _ } -> failwith "Heap is empty"
  | {size; elements = [] } -> failwith "Heap size != 0, but empty" 
  | {size = 1; elements = [x] } -> (x, empty)
  | {size; elements = x :: xs } ->    
      let rec get_last lst =
        match lst with
        | [] -> failwith "Error: unexpected empty list during remove_min"           
        | [y] -> y, []                                                              
        | y :: ys ->
            let last, rest = get_last ys in
            last, (y :: rest)
      in
      let last, rest = get_last xs in
      let new_elements = last :: rest in
      let new_size = size - 1 in
      let reversed_elements = List.rev new_elements in
      let new_heap = { size = new_size; elements = reversed_elements } in
      let heapified = min_heapify new_heap in
      (x, heapified)

