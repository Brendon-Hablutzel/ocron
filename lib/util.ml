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
