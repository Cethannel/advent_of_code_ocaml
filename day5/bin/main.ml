let file = "input.txt";;

let rec read_lines file =
  match In_channel.input_line file with
  | None -> []
  | Some line -> line :: read_lines file
;;

let get_start_state list = 
  let rec aux = function
  | [] -> ([], [])
  | h :: (t: string list) -> if String.length h = 0 then ([], t) else
    let (start_state, out_list) = aux t in
    (h :: start_state, out_list)
  in 

  let rec remove_last = function
    | [_] -> []
    | h :: t -> h :: remove_last t
    | [] -> []
  in

  let (out, body) = aux list in

  let out_list = remove_last out in

  (out_list, body)
;;

let window list len =
  let rec aux list acc out =
    match list with
    | [] -> List.rev acc :: out
    | h :: t -> if List.length acc = len
    then
      aux t [h] ((List.rev acc) :: out)
    else
      aux t (h :: acc) out
  in

  aux list [] [] |> List.rev
;;

exception BadEntry of char list;;
exception Empty;;

let process_start_state lines = 
  let process_col = function
    | [] -> raise Empty
    | _ :: letter :: _ :: ' ' :: _ -> letter
    | _ :: letter :: _ :: _ -> letter
    | h :: t -> raise @@ BadEntry (h :: t)
  in

  let rec process_line = function
    | [] -> []
    | h :: t -> process_col h :: process_line t
  in

  let rec aux stacks_list lines =
    match lines with
    | [] -> stacks_list
    | h :: t -> (window h 4 |> process_line) :: aux stacks_list t
   in

   List.map (fun list -> String.to_seq list |> List.of_seq) lines |> aux []
;;

let print_char_list list =
  let rec aux = function
    | [a] -> print_char a
    | h :: t -> let () = print_char h in let () = print_char ',' in aux t
    | [] -> ()
  in
  let () = print_char '[' in
  let () = aux list in
  let () = print_char ']' in
  print_newline ()
;;

let rec run_all f list =
  match list with
  | [] -> ()
  | h :: t -> let _ = f h in run_all f t
;;

exception Bad_State_Line of char list

let create_state start_tate =
  let rec process_line line out =
    match line, out with
    | [], [] -> out
    | l_h :: l_t, out_h :: out_t -> (l_h :: out_h) :: process_line l_t out_t
    | [], _ :: _ -> raise (Bad_State_Line line)
    | _ :: _, [] -> raise (Bad_State_Line line)
  in

  let rec aux (list: char list list) acc = 
    match list with
    | [] -> acc
    | h :: t -> aux t @@ process_line h acc
  in

  aux start_tate @@ List.init (List.hd start_tate |> List.length) (fun _ -> [])
    |> List.map @@ List.filter (fun c -> c != ' ')
    |> List.map List.rev
;;

let process_moves list =
  let process_line line =
    match line with
    | "move" :: amount
      :: "from" :: from
      :: "to" :: to_col :: _ -> int_of_string amount,
      int_of_string from,
      int_of_string to_col
    | _ -> raise Not_found
  in

  let rec aux list =
    match list with
    | [] -> []
    | h :: t -> (process_line (String.split_on_char ' ' h)) :: aux t
  in

  aux list
;;

let print_move move =
  let (amount, from_col, to_col) = move in
  let () = print_string "move " in
  let () = print_int amount in
  let () = print_string " from " in
  let () = print_int from_col in
  let () = print_string " to " in
  let () = print_int to_col in
  print_newline ()
;;

let rec drain_n n list =
  match list with
  | [] -> []
  | h :: _ when n = 1 -> [h]
  | h :: t -> drain_n (n - 1) t @ [h]
;;

let rec drain_n_rev n list =
  match list with
  | [] -> []
  | h :: _ when n = 1 -> [h]
  | h :: t -> h :: drain_n_rev (n - 1) t
;;

let rec pop_n n list =
    match list with
    | [] -> []
    | _ :: t when n = 1 -> t
    | _ :: t -> pop_n (n - 1) t
;;


let rec do_moves moves state =
  let update_state move state =
    let (amount, from_col, to_col) = move in
    let removed_list = List.mapi (fun i v -> if i = from_col - 1
        then pop_n amount v
        else v
      ) state in
    let removed_values = List.nth state (from_col - 1) |>
    drain_n amount in
    let out = List.mapi (fun i v -> if i = (to_col - 1)
        then removed_values @ v
        else v
      ) removed_list in
    let _ = run_all print_char_list out in
    let _ = print_newline () in
    out
  in

  match moves with
  | [] -> state
  | h :: t -> do_moves t @@ update_state h state
;;

let rec do_moves_part_2 moves state =
  let update_state move state =
    let (amount, from_col, to_col) = move in
    let removed_list = List.mapi (fun i v -> if i = from_col - 1
        then pop_n amount v
        else v
      ) state in
    let removed_values = List.nth state (from_col - 1) |>
    drain_n_rev amount in
    let out = List.mapi (fun i v -> if i = (to_col - 1)
        then removed_values @ v
        else v
      ) removed_list in
    let _ = run_all print_char_list out in
    let _ = print_newline () in
    out
  in

  match moves with
  | [] -> state
  | h :: t -> do_moves_part_2 t @@ update_state h state
;;


let () =
  let ic = open_in file in
  let lines = read_lines ic in
  let (start_state_lines, move_lines) = get_start_state lines in
  let () = close_in ic in
  let start_state = process_start_state start_state_lines in
  let _ = run_all print_char_list start_state in
  let state = create_state start_state in
  let _ = run_all print_char_list state in
  let moves = process_moves move_lines in
  let _ = run_all print_move moves in
  let end_state = do_moves moves state in
  let out = List.map List.hd end_state |> List.to_seq |> String.of_seq in
  let () = print_endline out in
  let end_state = do_moves_part_2 moves state in
  let out_2 = List.map List.hd end_state |> List.to_seq |> String.of_seq in
  print_endline out_2
;;
