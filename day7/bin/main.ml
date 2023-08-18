let file = "input.txt";;

let rec read_lines file =
  match In_channel.input_line file with
  | None -> []
  | Some line -> line :: read_lines file
;;

type dir_elem = 
  | File of string * int
  | Dir of string * dir_elem list
;;

let create_filesystem input =
  let aux acc list =
    match list with
    | [] -> acc
    | h :: t -> match String.sub h 0 3 with
    
;;

let () = 
    let ic = open_in file in
    let data = read_lines ic in
    close_in ic
;;
