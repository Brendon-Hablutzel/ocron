let ( let* ) result map_func =
  match result with
  | Ok success_value -> map_func success_value
  | Error err -> Error err

type cron_term =
  (* * *)
  | Any
  (* a single number *)
  | Single of int
  (* multiple comma-separated numbers *)
  | Multiple of int list
  (* a-b *)
  | Range of int * int
  (* */n, where this int is n *)
  | Interval of int

type cron_schedule = {
  minute : cron_term;
  hour : cron_term;
  day_of_month : cron_term;
  month : cron_term;
  day_of_week : cron_term;
}

let any_re = Str.regexp "^\\*$"
let single_re = Str.regexp "^\\([0-9]+\\)$"
let range_re = Str.regexp "^\\([0-9]+\\)-\\([0-9]+\\)$"
let interval_re = Str.regexp "^\\*\\/\\([0-9]+\\)$"

(* TODO: look into the re library, might be a safer *)
let parse_cron_term min_val max_val str_term =
  match str_term with
  (* any *)
  | str_term when Str.string_match any_re str_term 0 -> Ok Any
  (* single *)
  | str_term when Str.string_match single_re str_term 0 ->
      (* these could throw, but shouldn't, so it's not in a try...with *)
      let single = int_of_string @@ Str.matched_group 1 str_term in
      if single >= min_val && single <= max_val then Ok (Single single)
      else
        Error
          (Printf.sprintf
             "single term value out of allowed range: %d should be between %d \
              and %d"
             single min_val max_val)
  (* multiple *)
  (* NOTE: can't match on comma-separated list of arbitrary length with ocaml regexp *)
  | str_term when String.contains str_term ',' -> (
      let split = String.split_on_char ',' str_term in
      match Util.map_option int_of_string_opt split with
      | Some values -> Ok (Multiple values)
      (* TODO: use map_result instead to get specific integer? *)
      | None ->
          Error
            (Printf.sprintf
               "one or more values in a comma-separated list term were not \
                valid integers: %s"
               str_term))
  (* range *)
  | str_term when Str.string_match range_re str_term 0 ->
      (* again, these could throw, but shouldn't, since the regex only matches valid integers *)
      let range_begin = int_of_string @@ Str.matched_group 1 str_term in
      let range_end = int_of_string @@ Str.matched_group 2 str_term in
      if
        range_begin >= min_val && range_begin <= max_val && range_end >= min_val
        && range_end <= max_val
      then Ok (Range (range_begin, range_end))
      else
        Error
          (Printf.sprintf
             "one or both range term values out of allowed range: %d and %d \
              should each be betweeen %d and %d"
             range_begin range_end min_val max_val)
  (* interval *)
  | str_term when Str.string_match interval_re str_term 0 ->
      let interval = int_of_string @@ Str.matched_group 1 str_term in
      if interval > 0 then Ok (Interval interval)
      else
        Error
          (Printf.sprintf "interval value must be positive: got %d" interval)
  | _ ->
      Error
        (Printf.sprintf "cron term did not match any known patterns: %s"
           str_term)

let parse_minute_term = parse_cron_term 0 59
let parse_hour_term = parse_cron_term 0 23
let parse_day_of_month_term = parse_cron_term 1 31
let parse_month_term = parse_cron_term 1 12
let parse_day_of_week_term = parse_cron_term 0 6

let parse_cron_schedule string_cron_expr =
  let string_terms = String.split_on_char ' ' string_cron_expr in
  match string_terms with
  | [ minute_str; hour_str; day_of_month_str; month_str; day_of_week_str ] ->
      let* minute = parse_minute_term minute_str in
      let* hour = parse_hour_term hour_str in
      let* day_of_month = parse_day_of_month_term day_of_month_str in
      let* month = parse_month_term month_str in
      let* day_of_week = parse_day_of_week_term day_of_week_str in
      Ok { minute; hour; day_of_month; month; day_of_week }
  | _ -> Error "cron schedule should have exactly 5 space-separated terms"
