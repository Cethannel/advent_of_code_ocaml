let file = "input.txt";;

module CS = Set.Make(Char);;

let get_start_packet list =
  let rec aux acc pos list =
    match list with
    | [] -> raise Not_found
    | h :: t when List.length acc < 3 -> 
        aux (acc @ [h]) (pos + 1) t
    | h :: t when CS.of_list (h :: acc) |> CS.to_seq |> Seq.length < 4 -> (
      match acc with
        | [] -> aux [h] (pos + 1) t
        | _ :: acc_t -> aux (acc_t @ [h]) (pos + 1) t
      )
    | _ -> pos
  in

    aux [] 1 list
;;

let get_message_start_packet list =
  let rec aux acc pos list =
    match list with
    | [] -> raise Not_found
    | h :: t when List.length acc < 13 -> 
        aux (acc @ [h]) (pos + 1) t
    | h :: t when CS.of_list (h :: acc) |> CS.to_seq |> Seq.length < 14 -> (
      match acc with
        | [] -> aux [h] (pos + 1) t
        | _ :: acc_t -> aux (acc_t @ [h]) (pos + 1) t
      )
    | _ -> pos
  in

    aux [] 1 list
;;


let () = 
    let ic = open_in file in
    let data = In_channel.input_all ic |> String.to_seq |> List.of_seq in
    let start_packet = get_start_packet data in
    let () = print_int start_packet in
    let () = print_newline () in
    let message_start = get_message_start_packet data in
    let () = print_int message_start in
    let () = print_newline () in
    close_in ic
;;
