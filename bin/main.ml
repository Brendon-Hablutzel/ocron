let () =
  let cron_schedule = Ocron.Cron.parse_cron_schedule "* * * * *" in
  match cron_schedule with
  | Ok _ -> print_endline "success"
  | Error err -> print_endline err
