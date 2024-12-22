let file_name = "temp_file.txt"

let read_file fname = 
    let stream = open_in fname in
    let rec loop i_ch acc = 
        try 
            let line = ( input_line i_ch ) ^ "\n" in
            loop i_ch (line :: acc)
        with e -> (
            acc |> List.rev
            )
    in
    let str_arr = loop stream [] in
    let rec concatene acc = function
        | [] -> acc 
        | h :: t -> concatene ( acc ^ h ) t
    in
    concatene "" str_arr

let read_file2 fname = 
    let in_ch = open_in fname in
    let istr = Bs.of_in_channel in_ch in
    istr

let res = read_file file_name
let () = Printf.printf "%s\n" res
