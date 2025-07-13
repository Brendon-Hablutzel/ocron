let readable_cron_term_value term_value =
  match term_value with
  | Cron_parsing.Any -> "any"
  | Cron_parsing.Single value -> Printf.sprintf "at %d" value
  | Cron_parsing.Many values ->
      "at any of " ^ String.concat ", " (List.map Int.to_string values)
  | Cron_parsing.Range (start, end_) ->
      Printf.sprintf "from %d to %d" start end_
  | Cron_parsing.Interval step -> Printf.sprintf "every %d" step

type cron_term_type = Minute | Hour | DayOfMonth | Month | DayOfWeek

let readable_cron_term_type term_type =
  match term_type with
  | Minute -> "minute"
  | Hour -> "hour"
  | DayOfMonth -> "day of month"
  | Month -> "month"
  | DayOfWeek -> "day of week"

let cron_term_index_to_type term_index =
  match term_index with
  | 0 -> Minute
  | 1 -> Hour
  | 2 -> DayOfMonth
  | 3 -> Month
  | 4 -> DayOfWeek
  | invalid ->
      failwith (Int.to_string invalid ^ " is not a valid cron term index")

type cron_token =
  | Comment of { line_index : int; content : string }
  | Term of {
      line_index : int;
      char_index : int;
      term_type : cron_term_type;
      term_value : (Cron_parsing.cron_term, string) result;
      content : string;
    }
  | Command of { line_index : int; char_index : int; content : string }

let _get_cron_token_content = function
  | Comment { content; _ } -> content
  | Term { content; _ } -> content
  | Command { content; _ } -> content

let parse_cron_term_value term term_type =
  match term_type with
  | Minute -> Cron_parsing.parse_minute_term term
  | Hour -> Cron_parsing.parse_hour_term term
  | DayOfMonth -> Cron_parsing.parse_day_of_month_term term
  | Month -> Cron_parsing.parse_month_term term
  | DayOfWeek -> Cron_parsing.parse_day_of_week_term term

let get_token_char_range cron_token =
  match cron_token with
  | Comment { content; _ } -> (0, String.length content)
  | Term { char_index; content; _ } ->
      (char_index, char_index + String.length content)
  | Command { char_index; content; _ } ->
      (char_index, char_index + String.length content)

let rec tokenize_line line_index char_index term_index rest_of_terms =
  if term_index = 5 then
    let rest_of_line = String.concat " " rest_of_terms in
    let command = Command { line_index; char_index; content = rest_of_line } in
    [ command ]
  else
    match rest_of_terms with
    | [] -> []
    | next_term :: remaining_terms ->
        let term_type = cron_term_index_to_type term_index in
        let term_value = parse_cron_term_value next_term term_type in
        let term =
          Term
            {
              line_index;
              char_index;
              term_type;
              term_value;
              content = next_term;
            }
        in
        term
        :: tokenize_line line_index
             (char_index + String.length next_term + 1)
             (term_index + 1) remaining_terms

let tokenize_entry line_index line =
  tokenize_line line_index 0 0 (String.split_on_char ' ' line)

let tokenize_comment line_index line = Comment { line_index; content = line }

let rec tokenize_lines line_index lines_to_parse =
  match lines_to_parse with
  | [] -> []
  | next_line :: remaining_lines ->
      let line_tokens =
        if String.length next_line = 0 then []
        else if String.starts_with ~prefix:"#" next_line then
          [ tokenize_comment line_index next_line ]
        else tokenize_entry line_index next_line
      in
      line_tokens @ tokenize_lines (line_index + 1) remaining_lines

let tokenize_file (contents : string) =
  let lines = String.split_on_char '\n' contents in
  tokenize_lines 0 lines

let get_line_tokens target_line_index tokens =
  List.filter
    (function
      | Comment { line_index; _ } -> line_index = target_line_index
      | Term { line_index; _ } -> line_index = target_line_index
      | Command { line_index; _ } -> line_index = target_line_index)
    tokens
