let rec map_option f list =
  match list with
  | [] -> Some []
  | head :: tail -> (
      match f head with
      | None -> None
      | Some some_head -> (
          match map_option f tail with
          | None -> None
          | Some some_tail -> Some (some_head :: some_tail)))

let rec map_result f list =
  match list with
  | [] -> Ok []
  | head :: tail -> (
      match f head with
      | Error err -> Error err
      | Ok ok_head -> (
          match map_result f tail with
          | Error err -> Error err
          | Ok ok_tail -> Ok (ok_head :: ok_tail)))

let get_username () =
  let uid = Unix.getuid () in
  let pw_entry = Unix.getpwuid uid in
  pw_entry.Unix.pw_name

(* let get_duplicates list =
  let sorted_jobs = List.sort compare list in
  let rec find_duplicates = function
    | [] | [ _ ] -> []
    | first_el :: second_el :: rest ->
        if first_el = second_el then first_el :: find_duplicates rest
        else find_duplicates rest
  in
  find_duplicates sorted_jobs *)

(* let rec extract_first pred lst =
  match lst with
  | [] -> None
  | x :: xs -> (
      if pred x then Some (x, xs)
      else
        match extract_first pred xs with
        | None -> None
        | Some (found, rest) -> Some (found, x :: rest)) *)
