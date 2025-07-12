open Cron_parsing

(* TODO: do some optimizations:
- precompute valid values
- skip over ranges with no valid values
*)

let is_wildcard = function Any -> true | _ -> false
let seconds_until_next_minute now = 60. -. mod_float now 60.

let time_component_satisfies time_component cron_term =
  match cron_term with
  | Any -> true
  | Single target -> time_component = target
  | Many targets -> List.mem time_component targets
  | Range (range_start, range_end) ->
      time_component >= range_start && time_component <= range_end
  | Interval interval -> time_component mod interval = 0

let time_satisfies now cron_schedule =
  let now = Unix.localtime now in
  let minute_satisfies =
    lazy (time_component_satisfies now.tm_min cron_schedule.minute)
  in
  let hour_satisfies =
    lazy (time_component_satisfies now.tm_hour cron_schedule.hour)
  in
  let day_of_month_satisfies =
    lazy (time_component_satisfies now.tm_mday cron_schedule.day_of_month)
  in
  let month_satisfies =
    lazy (time_component_satisfies (now.tm_mon + 1) cron_schedule.month)
  in
  let day_of_week_satisfies =
    lazy (time_component_satisfies now.tm_wday cron_schedule.day_of_week)
  in
  let day_field_satisfies =
    lazy
      (match
         ( is_wildcard cron_schedule.day_of_month,
           is_wildcard cron_schedule.day_of_week )
       with
      | false, false ->
          Lazy.force day_of_month_satisfies || Lazy.force day_of_week_satisfies
      | false, true -> Lazy.force day_of_month_satisfies
      | true, false -> Lazy.force day_of_week_satisfies
      | true, true -> true)
  in
  Lazy.force minute_satisfies
  && Lazy.force hour_satisfies && Lazy.force month_satisfies
  && Lazy.force day_field_satisfies

(* TODO: potentially some way to limit how far this looks in the future *)
let calculate_next_execution_time now schedule =
  let next_minute = now +. seconds_until_next_minute now in
  let rec find_execution_time current_time =
    if time_satisfies current_time schedule then current_time
    else find_execution_time (current_time +. 60.)
  in
  find_execution_time next_minute
