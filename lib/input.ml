open Unix

(* TODO: maybe move this into cli directory since it won't be used in the daemon *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let red s = Printf.sprintf "\027[31m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let blue s = Printf.sprintf "\027[34m%s\027[0m" s
let magenta s = Printf.sprintf "\027[35m%s\027[0m" s
let cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let white s = Printf.sprintf "\027[37m%s\027[0m" s
let bright_black s = Printf.sprintf "\027[90m%s\027[0m" s
let bright_red s = Printf.sprintf "\027[91m%s\027[0m" s
let bright_green s = Printf.sprintf "\027[92m%s\027[0m" s
let bright_yellow s = Printf.sprintf "\027[93m%s\027[0m" s
let bright_blue s = Printf.sprintf "\027[94m%s\027[0m" s
let bright_magenta s = Printf.sprintf "\027[95m%s\027[0m" s
let bright_cyan s = Printf.sprintf "\027[96m%s\027[0m" s
let bright_white s = Printf.sprintf "\027[97m%s\027[0m" s

(* ANSI escape codes to hide/show cursor *)
let hide_cursor () =
  Printf.printf "\027[?25l";
  flush Stdlib.stdout

let show_cursor () =
  Printf.printf "\027[?25h";
  flush Stdlib.stdout

(* Terminal raw mode setup and teardown *)
let with_raw_mode f =
  let fd = Unix.stdin in
  let orig = Unix.tcgetattr fd in
  let raw =
    { orig with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 }
  in

  let restore () =
    Unix.tcsetattr fd Unix.TCSADRAIN orig;
    show_cursor ();
    flush Stdlib.stdout
  in

  let sigint_handler =
    Sys.Signal_handle
      (fun _signum ->
        restore ();
        (* Use exit 130 (standard for SIGINT) *)
        exit 130)
  in

  Sys.set_signal Sys.sigint sigint_handler;

  Unix.tcsetattr fd Unix.TCSADRAIN raw;
  hide_cursor ();
  flush Stdlib.stdout;

  try
    let result = f () in
    restore ();
    Sys.set_signal Sys.sigint Sys.Signal_default;
    result
  with e ->
    restore ();
    Sys.set_signal Sys.sigint Sys.Signal_default;
    raise e

(* Move cursor up by n lines *)
let move_cursor_up n =
  if n > 0 then Printf.printf "\027[%dA" n;
  flush Stdlib.stdout

(* Erase the current line *)
let erase_line () =
  Printf.printf "\027[2K\r";
  flush Stdlib.stdout

(* Render menu *)
let render_select_many items cursor selected to_string =
  move_cursor_up (Array.length items);
  for i = 0 to Array.length items - 1 do
    erase_line ();
    let pointer = if i = cursor then cyan ">" else " " in
    let mark = if List.mem i selected then green "[x]" else "[ ]" in
    Printf.printf "%s %s %s\n" pointer mark (to_string items.(i))
  done;
  flush Stdlib.stdout

let read_key () =
  let buf = Bytes.create 3 in
  let n = input Stdlib.stdin buf 0 3 in
  if n = 1 then
    match Bytes.get buf 0 with
    | '\027' -> `Escape
    | '\n' -> `Enter
    | ' ' -> `Space
    | '\003' -> `CtrlC
    | c -> `Char c
  else if n = 3 then
    if Bytes.get buf 0 = '\027' && Bytes.get buf 1 = '[' then
      match Bytes.get buf 2 with 'A' -> `Up | 'B' -> `Down | _ -> `Unknown
    else `Unknown
  else `Unknown

let select_many_interactive_menu items to_string =
  let cursor = ref 0 in
  let selected = ref [] in

  Array.iteri
    (fun i item ->
      let pointer = if i = !cursor then cyan ">" else " " in
      let mark = if List.mem i !selected then green "[x]" else "[ ]" in
      Printf.printf "%s %s %s\n" pointer mark (to_string item))
    items;
  flush Stdlib.stdout;

  let rec loop () =
    match read_key () with
    | `Up ->
        cursor := if !cursor = 0 then Array.length items - 1 else !cursor - 1;
        render_select_many items !cursor !selected to_string;
        loop ()
    | `Down ->
        cursor := (!cursor + 1) mod Array.length items;
        render_select_many items !cursor !selected to_string;
        loop ()
    | `Space ->
        if List.mem !cursor !selected then
          selected := List.filter (( <> ) !cursor) !selected
        else selected := !cursor :: !selected;
        render_select_many items !cursor !selected to_string;
        loop ()
    | `Enter -> !selected |> List.sort compare |> List.map (fun i -> items.(i))
    | `CtrlC -> []
    | _ -> loop ()
  in
  loop ()

let user_select_many_from items to_string =
  with_raw_mode (fun () -> select_many_interactive_menu items to_string)

let render_select_one items cursor to_string =
  move_cursor_up (Array.length items);
  for i = 0 to Array.length items - 1 do
    erase_line ();
    let pointer = if i = cursor then cyan ">" else " " in
    Printf.printf "%s %s\n" pointer (to_string items.(i))
  done;
  flush Stdlib.stdout

let select_one_interactive_menu items to_string =
  if Array.length items = 0 then None
  else
    let cursor = ref 0 in
    let selected = ref (Array.get items !cursor) in

    Array.iteri
      (fun i item ->
        let pointer = if i = !cursor then ">" else " " in
        Printf.printf "%s %s\n" (cyan pointer) (to_string item))
      items;
    flush Stdlib.stdout;

    let rec loop () =
      match read_key () with
      | `Up ->
          cursor := if !cursor = 0 then Array.length items - 1 else !cursor - 1;
          render_select_one items !cursor to_string;
          loop ()
      | `Down ->
          cursor := (!cursor + 1) mod Array.length items;
          render_select_one items !cursor to_string;
          loop ()
      | `Space | `Enter -> Some !selected
      | `CtrlC -> None
      | _ -> loop ()
    in
    loop ()

let user_select_one_from items to_string =
  with_raw_mode (fun () -> select_one_interactive_menu items to_string)

let select_boolean prompt =
  Printf.printf "%s" prompt;
  flush Stdlib.stdout;

  let rec loop () =
    match read_key () with
    | `Char 'y' | `Enter ->
        print_endline "✓";
        Some true
    | `Char 'n' ->
        print_endline "✗";
        Some false
    | `CtrlC ->
        print_newline ();
        None
    | _ -> loop ()
  in
  loop ()

let user_select_boolean prompt = with_raw_mode (fun () -> select_boolean prompt)

let clear_lines n =
  for _ = 1 to n do
    print_string "\027[1A";
    (* move cursor up *)
    print_string "\027[2K"
    (* clear line *)
  done

let pad_right width s =
  let len = String.length s in
  if len >= width then s else s ^ String.make (width - len) ' '
