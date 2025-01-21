let read_for_compression fname = 
  let ic = open_in fname in
  let rec loop acc =
      try
          let byte = input_byte ic in
          loop (byte :: acc)
      with End_of_file ->
          close_in ic;
          acc
  in
  loop [] 