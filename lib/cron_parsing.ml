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
  | Many of int list
  (* a-b *)
  | Range of int * int
  (* */n, where this int is n *)
  | Interval of int

let cron_term_string term =
  match term with
  | Any -> "*"
  | Single n -> Int.to_string n
  | Many ns -> String.concat "," (List.map Int.to_string ns)
  | Range (from, until) -> Printf.sprintf "%d-%d" from until
  | Interval nth -> Printf.sprintf "*/%d" nth

type cron_schedule = {
  minute : cron_term;
  hour : cron_term;
  day_of_month : cron_term;
  month : cron_term;
  day_of_week : cron_term;
}

let cron_schedule_string schedule =
  Printf.sprintf "%s %s %s %s %s"
    (cron_term_string schedule.minute)
    (cron_term_string schedule.hour)
    (cron_term_string schedule.day_of_month)
    (cron_term_string schedule.month)
    (cron_term_string schedule.day_of_week)

(* required for deriving yojson on job *)
let cron_schedule_to_yojson schedule = `String (cron_schedule_string schedule)
let any_re = Str.regexp "^\\*$"
let single_re = Str.regexp "^\\([0-9]+\\)$"
let range_re = Str.regexp "^\\([0-9]+\\)-\\([0-9]+\\)$"
let interval_re = Str.regexp "^\\*\\/\\([0-9]+\\)$"

(* TODO: look into the re library, might be safer *)
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
      | Some values -> Ok (Many values)
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
      (* TODO: err if end is after begin *)
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

let cron_schedule_of_yojson json =
  match json with
  | `String str -> parse_cron_schedule str
  | _ -> Error "expected string for schedule"

type crontab_entry = { schedule : cron_schedule; command : string }

let parse_crontab_entry entry =
  match String.split_on_char ' ' entry with
  | minute :: hour :: day_of_month :: month :: day_of_week :: rest -> (
      match
        parse_cron_schedule
          (Printf.sprintf "%s %s %s %s %s" minute hour day_of_month month
             day_of_week)
      with
      | Ok schedule -> Ok { schedule; command = String.concat " " rest }
      | Error err -> Error err)
  | _ ->
      Error
        "invalid crontab entry: should have a valid expression followed by a \
         command"

(* type cron_schedule = {
  minute : cron_term;
  hour : cron_term;
  day_of_month : cron_term;
  month : cron_term;
  day_of_week : cron_term;
} *)

(* type cron_term =
  (* * *)
  | Any
  (* a single number *)
  | Single of int
  (* multiple comma-separated numbers *)
  | Many of int list
  (* a-b *)
  | Range of int * int
  (* */n, where this int is n *)
  | Interval of int *)

let get_ordinal_suffix number =
  if number = 1 then "st" else if number = 2 then "nd" else "th"

let with_ordinal_suffix num =
  let suffix = get_ordinal_suffix num in
  Printf.sprintf "%d%s" num suffix

let int_to_readable_list int_list =
  String.concat ", " (List.map Int.to_string int_list)

let left_pad target_digits num =
  let str_num = Int.to_string num in
  let to_pad = target_digits - String.length str_num in
  if to_pad > 0 then
    let padding = String.make to_pad '0' in
    padding ^ str_num
  else str_num

let left_pad_time hour_or_minute = left_pad 2 hour_or_minute

let readable_cron_schedule schedule =
  let get_hour_or_minute_readable name term_value =
    match term_value with
    | Any -> Printf.sprintf "every %s" name
    | Single minute -> Printf.sprintf "%s %d" name minute
    | Many minutes ->
        let minutes_string = int_to_readable_list minutes in
        Printf.sprintf "%ss %s" name minutes_string
    | Range (start_minute, end_minute) ->
        Printf.sprintf "%ss %d to %d" name start_minute end_minute
    | Interval minute_step ->
        Printf.sprintf "every %s %s" (with_ordinal_suffix minute_step) name
  in

  let get_hour_readable = get_hour_or_minute_readable "hour" in
  let get_minute_readable = get_hour_or_minute_readable "minute" in

  let hour_minute_readable =
    match (schedule.minute, schedule.hour) with
    | minute, Any -> get_minute_readable minute
    | Single minute, Single hour ->
        let padded_minute = left_pad_time minute in
        let padded_hour = left_pad_time hour in
        Printf.sprintf "%s:%s" padded_minute padded_hour
    | minute, hour ->
        let minute_readable = get_minute_readable minute in
        let hour_readable = get_hour_readable hour in
        minute_readable ^ " during " ^ hour_readable
  in

  let month_num_to_name month =
    match month with
    | 1 -> "January"
    | 2 -> "February"
    | 3 -> "March"
    | 4 -> "April"
    | 5 -> "May"
    | 6 -> "June"
    | 7 -> "July"
    | 8 -> "August"
    | 9 -> "September"
    | 10 -> "October"
    | 11 -> "November"
    | 12 -> "December"
    | _ -> failwith (Printf.sprintf "invalid month %d" month)
  in

  let day_of_week_to_name day_of_week =
    match day_of_week with
    | 0 -> "Sunday"
    | 1 -> "Monday"
    | 2 -> "Tuesday"
    | 3 -> "Wednesday"
    | 4 -> "Thursday"
    | 5 -> "Friday"
    | 6 -> "Saturday"
    | _ -> failwith (Printf.sprintf "invalid day of week %d" day_of_week)
  in

  let get_day_of_month_readable dom_term_value =
    match dom_term_value with
    | Any -> None
    | Single dom -> Some (Printf.sprintf "the %s day" (with_ordinal_suffix dom))
    | Many doms ->
        let string_doms = List.map with_ordinal_suffix doms in
        let doms_string = String.concat ", " string_doms in
        Some (Printf.sprintf "the %s days" doms_string)
    | Range (start_dom, end_dom) ->
        Some
          (Printf.sprintf "the %s to %s days"
             (with_ordinal_suffix start_dom)
             (with_ordinal_suffix end_dom))
    | Interval dom_step ->
        Some (Printf.sprintf "every %s day" (with_ordinal_suffix dom_step))
  in

  let get_month_readable month_term_value =
    match month_term_value with
    | Any -> None
    | Single month -> Some (month_num_to_name month)
    | Many months ->
        let string_months = List.map month_num_to_name months in
        let months_string = String.concat ", " string_months in
        Some months_string
    | Range (start_month, end_month) ->
        Some
          (Printf.sprintf "%s to %s"
             (month_num_to_name start_month)
             (month_num_to_name end_month))
    | Interval month_step ->
        Some (Printf.sprintf "every %s month" (with_ordinal_suffix month_step))
  in

  let get_day_of_week_readable dow_term_value =
    match dow_term_value with
    | Any -> None
    | Single dow -> Some (day_of_week_to_name dow)
    | Many dows ->
        let string_dows = List.map day_of_week_to_name dows in
        let dows_string = String.concat ", " string_dows in
        Some dows_string
    | Range (start_dow, end_dow) ->
        Some
          (Printf.sprintf "%s to %s"
             (day_of_week_to_name start_dow)
             (day_of_week_to_name end_dow))
    | Interval dow_step ->
        Some
          (Printf.sprintf "every %s day of the week"
             (with_ordinal_suffix dow_step))
  in

  let day_of_month_readable = get_day_of_month_readable schedule.day_of_month in
  let month_readable = get_month_readable schedule.month in
  let day_of_week_readable = get_day_of_week_readable schedule.day_of_week in

  let day_and_month_readable =
    match (day_of_month_readable, month_readable, day_of_week_readable) with
    | None, None, None -> None
    | Some dom, None, None ->
        Some (Printf.sprintf "on %s of the month" dom) (* TODO: check this *)
    | None, Some month, None -> Some (Printf.sprintf "in %s" month)
    | None, None, Some dow -> Some (Printf.sprintf "on %s" dow)
    | Some dom, Some month, None ->
        Some (Printf.sprintf "on %s in %s" dom month)
    | None, Some month, Some dow ->
        Some (Printf.sprintf "on %s in %s" dow month)
    | Some dom, None, Some dow ->
        Some (Printf.sprintf "on %s of the month and on %s" dom dow)
    | Some dom, Some month, Some dow ->
        Some (Printf.sprintf "on %s and on %s in %s" dom dow month)
  in

  "at " ^ hour_minute_readable
  ^
  match day_and_month_readable with
  | Some day_and_month_readable -> " " ^ day_and_month_readable
  | None -> ""
