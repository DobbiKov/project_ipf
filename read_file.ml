
let read_file_in_bytes_for_compression fname = 
    let in_ch = open_in fname in
    let istr = Bs.of_in_channel in_ch in
    let rec loop i_str acc =
        try 
            let byte = ( Bs.read_byte i_str ) in
            (*Printf.printf "textim: %d | " (byte);*)
            (*Printf.printf "textim: %c\n" (( byte ) |> Char.chr );*)
            loop i_str (byte :: acc)
        with 
        | _ -> 
            close_in in_ch;
            acc
    in
    (loop istr []) |> List.rev
