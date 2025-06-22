(* utilies for migrating from cron *)

let read_all_lines ic =
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file -> List.rev acc
  in
  loop []

let get_current_user_crontab_entries () =
  let ic = Unix.open_process_in "crontab -l" in
  let lines = read_all_lines ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED code when code == 1 ->
      (* cron returns exit code 1 when no crontab found *)
      Ok []
  | Unix.WEXITED code when code > 1 ->
      Error (Printf.sprintf "process exited with code %d" code)
  | Unix.WSIGNALED signal ->
      Error (Printf.sprintf "process killed by signal %d" signal)
  | Unix.WSTOPPED signal ->
      Error (Printf.sprintf "process stopped by signal %d" signal)
  | _ ->
      let non_comment_lines =
        List.filter
          (fun line -> not (String.starts_with ~prefix:"#" line))
          lines
      in
      Ok non_comment_lines
