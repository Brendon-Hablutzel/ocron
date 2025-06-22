open Cmdliner
open Ocron
open Ocron.Config

let ( let* ) result map_func =
  match result with
  | Ok success_value -> map_func success_value
  | Error err -> Error (`Msg (Input.red err))

(* TODO: output format options (json for now) *)
(* TODO: options for colors *)

let user_select_one_job_from filter_fn no_jobs_found_message =
  match list_jobs () with
  | Ok (Some jobs) -> (
      let jobs = List.filter filter_fn jobs in
      if List.length jobs = 0 then Error no_jobs_found_message
      else
        let selected =
          Input.user_select_one_from (Array.of_list jobs) (fun job -> job.name)
        in
        Input.clear_lines (List.length jobs);
        flush stdout;
        match selected with
        | Some selected_job -> Ok selected_job
        | None -> Error "exited without selecting job")
  | Ok None -> Error "no config file found"
  | Error err -> Error err

let get_or_user_select_job_from name filter_fn no_jobs_found_message =
  match name with
  | Some name -> (
      match get_job name with
      | Ok (Some job) -> Ok job
      | Ok None -> Error "no job found"
      | Error err -> Error err)
  | None -> user_select_one_job_from filter_fn no_jobs_found_message

let get_or_user_select_job name =
  get_or_user_select_job_from name
    (fun _ -> true)
    "no jobs found in config file"

let get_or_user_select_enabled_job name =
  get_or_user_select_job_from name
    (fun job -> job.enabled)
    "no enabled jobs found"

let get_or_user_select_disabled_job name =
  get_or_user_select_job_from name
    (fun job -> not job.enabled)
    "no disabled jobs found"

let printable_job_with_name_padding name_width job =
  let enabled_symbol = if job.enabled then Input.green "✓" else Input.red "✗" in
  Printf.sprintf "%s  %s  %s  %s" enabled_symbol
    (Input.cyan (Input.pad_right name_width job.name))
    (Cron_parsing.cron_schedule_string job.schedule)
    job.command

let printable_multiple_jobs jobs =
  let longest_job_name_len =
    List.fold_left
      (fun acc job ->
        if String.length job.name > acc then String.length job.name else acc)
      0 jobs
  in
  let job_printer = printable_job_with_name_padding longest_job_name_len in
  String.concat "\n" (List.map job_printer jobs)

let printable_job job =
  printable_job_with_name_padding (String.length job.name) job

let get_arrow_diff job_1 job_2 =
  let max_name_length =
    max (String.length job_1.name) (String.length job_2.name)
  in
  let enabled_symbol =
    if job_1.enabled = job_2.enabled then " " else Input.yellow "↓"
  in
  let name_symbol = if job_1.name = job_2.name then " " else Input.yellow "↓" in
  let schedule_symbol =
    if job_1.schedule = job_2.schedule then " " else Input.yellow "↓"
  in
  let command_symbol =
    if job_1.command = job_2.command then " " else Input.yellow "↓"
  in
  let padded_name_symbol = Input.pad_right max_name_length name_symbol in
  let padded_schedule_symbol = Input.pad_right 9 schedule_symbol in
  Printf.sprintf "%s  %s  %s  %s" enabled_symbol padded_name_symbol
    padded_schedule_symbol command_symbol

let yes_flag =
  let doc = "Whether to bypass interactive confirmations" in
  Arg.(value & flag & info [ "y"; "yes" ] ~doc)

let jobs_list () =
  match list_jobs () with
  | Ok (Some jobs) ->
      print_endline
        (Printf.sprintf "%d jobs found (%d enabled)" (List.length jobs)
           (List.length (List.filter (fun job -> job.enabled) jobs)));

      print_endline (printable_multiple_jobs jobs);
      Ok ()
  | Ok None -> Error (`Msg (Input.red "no config file found"))
  | Error err -> Error (`Msg (Input.red err))

let jobs_list_cmd =
  let info = Cmd.info "list" ~doc:"List all jobs" in
  Cmd.v info Term.(term_result (const jobs_list $ const ()))

let optional_name_first_arg =
  let doc = "Name of the job" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

(* TODO: show previous and next invocation time, and previous status (exit code and duration) *)
let jobs_get name =
  let* job = get_or_user_select_job name in
  print_endline (printable_job job);
  Ok ()

let jobs_get_cmd =
  let info = Cmd.info "get" ~doc:"Get config for a job" in
  Cmd.v info Term.(term_result (const jobs_get $ optional_name_first_arg))

let jobs_delete name yes =
  let* job = get_or_user_select_job name in
  let confirmed =
    yes
    || Option.value
         (Input.user_select_boolean "Confirm job deletion (y/n): ")
         ~default:false
  in
  if confirmed then (
    let* deleted_job = delete_job job.name in
    (match deleted_job with
    | Some _ -> print_endline (Input.green "Deleted job successfully")
    | None -> print_endline "No job deleted");
    Ok ())
  else Error (`Msg (Input.yellow "skipping deletion"))

let jobs_delete_cmd =
  let info = Cmd.info "delete" ~doc:"Delete a job" in
  Cmd.v info
    Term.(term_result (const jobs_delete $ optional_name_first_arg $ yes_flag))

let jobs_enable name yes =
  let* job = get_or_user_select_disabled_job name in
  if job.enabled then Error (`Msg "job already enabled")
  else
    let confirmed =
      yes
      || Option.value
           (Input.user_select_boolean "Confirm enabling job (y/n): ")
           ~default:false
    in
    if confirmed then (
      let* _ = set_job_enabled_status job.name true in
      print_endline (Input.green "Enabled job successfully");
      Ok ())
    else Error (`Msg (Input.yellow "skipping enabling"))

let jobs_enable_cmd =
  let info = Cmd.info "enable" ~doc:"Enable a job" in
  Cmd.v info
    Term.(term_result (const jobs_enable $ optional_name_first_arg $ yes_flag))

let jobs_disable name yes =
  let* job = get_or_user_select_enabled_job name in
  if job.enabled then Error (`Msg "job already disabled")
  else
    let confirmed =
      yes
      || Option.value
           (Input.user_select_boolean "Confirm disabling job (y/n): ")
           ~default:false
    in
    if confirmed then (
      let* _ = set_job_enabled_status job.name false in
      print_endline (Input.green "Disabled job successfully");
      Ok ())
    else Error (`Msg (Input.yellow "skipping disabling"))

let jobs_disable_cmd =
  let info = Cmd.info "disable" ~doc:"Disable a job" in
  Cmd.v info
    Term.(term_result (const jobs_disable $ optional_name_first_arg $ yes_flag))

let new_name_flag =
  let doc = "The new name for the job" in
  Arg.(
    value & opt (some string) None & info [ "n"; "name" ] ~docv:"NEW-NAME" ~doc)

let new_schedule_flag =
  let doc = "The new schedule for the job" in
  Arg.(
    value
    & opt (some string) None
    & info [ "s"; "schedule" ] ~docv:"NEW-SCHEDULE" ~doc)

let new_command_flag =
  let doc = "The new command for the job" in
  Arg.(
    value
    & opt (some string) None
    & info [ "c"; "command" ] ~docv:"NEW-COMMAND" ~doc)

let new_enabled_flag =
  let doc = "The new enabled status for the job" in
  Arg.(
    value
    & opt (some bool) None
    & info [ "e"; "enabled" ] ~docv:"NEW-ENABLED" ~doc)

let jobs_update current_name new_name new_schedule new_command new_enabled yes =
  match get_or_user_select_job current_name with
  | Ok current_job ->
      let new_job =
        Option.value
          (Option.map
             (fun new_name -> { current_job with name = new_name })
             new_name)
          ~default:current_job
      in
      let* new_job =
        match Option.map Cron_parsing.parse_cron_schedule new_schedule with
        | Some (Ok new_schedule) -> Ok { new_job with schedule = new_schedule }
        | None -> Ok new_job
        | Some (Error err) -> Error err
      in
      let new_job =
        Option.value
          (Option.map
             (fun new_command -> { new_job with command = new_command })
             new_command)
          ~default:new_job
      in
      let new_job =
        Option.value
          (Option.map
             (fun new_enabled -> { new_job with enabled = new_enabled })
             new_enabled)
          ~default:new_job
      in
      (* TODO: if (new_)job is the same as old job, use interactive prompt *)
      if current_job != new_job then (
        print_endline (printable_job current_job);
        print_endline (get_arrow_diff current_job new_job);
        print_endline (printable_job new_job);
        print_newline ();
        if
          yes
          || Option.value
               (Input.user_select_boolean "Confirm job update (y/n):")
               ~default:false
        then
          let* _ = put_job current_job.name new_job in
          let () = print_endline "Updated job successfully" in
          Ok ()
        else Error (`Msg (Input.yellow "skipping update, job not modified")))
      else Error (`Msg (Input.yellow "no changes, job not modified"))
  | Error err -> Error (`Msg err)

let jobs_update_cmd =
  let info = Cmd.info "update" ~doc:"Update a job's config interactively" in
  Cmd.v info
    Term.(
      term_result
        (const jobs_update $ optional_name_first_arg $ new_name_flag
       $ new_schedule_flag $ new_command_flag $ new_enabled_flag $ yes_flag))

let required_name_first_arg =
  let doc = "Name of the job" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let required_new_name_second_arg =
  let doc = "New name for the job" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"NEW-NAME" ~doc)

let required_new_schedule_third_arg =
  let doc = "New schedule for the job" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"NEW-SCHEDULE" ~doc)

let required_new_command_fourth_arg =
  let doc = "New command for the job" in
  Arg.(required & pos 3 (some string) None & info [] ~docv:"NEW-COMMAND" ~doc)

let required_new_enabled_fifth_arg =
  let doc = "New enabled status for the job" in
  Arg.(required & pos 4 (some bool) None & info [] ~docv:"NEW-ENABLED" ~doc)

let jobs_put current_name new_name new_schedule new_command new_enabled yes =
  match get_job current_name with
  | Ok (Some current_job) ->
      let* new_schedule = Cron_parsing.parse_cron_schedule new_schedule in
      let new_job =
        {
          name = new_name;
          schedule = new_schedule;
          command = new_command;
          enabled = new_enabled;
        }
      in
      if current_job != new_job then (
        print_endline (printable_job current_job);
        print_endline (get_arrow_diff current_job new_job);
        print_endline (printable_job new_job);
        print_newline ();
        if
          yes
          || Option.value
               (Input.user_select_boolean "Confirm job overwrite (y/n):")
               ~default:false
        then
          match put_job current_name new_job with
          | Ok _ -> Ok ()
          | Error err -> Error (`Msg (Input.red err))
        else Error (`Msg (Input.yellow "Skipping update")))
      else Error (`Msg (Input.yellow "No change, job not updated"))
  | Ok None ->
      Error (`Msg (Input.yellow "Job not found, no operation performed"))
  | Error err -> Error (`Msg (Input.red err))

let jobs_put_cmd =
  let info =
    Cmd.info "put" ~doc:"Overwrite an existing job with the given config"
  in
  Cmd.v info
    Term.(
      term_result
        (const jobs_put $ required_name_first_arg $ required_new_name_second_arg
       $ required_new_schedule_third_arg $ required_new_command_fourth_arg
       $ required_new_enabled_fifth_arg $ yes_flag))

let required_schedule_second_arg =
  let doc = "Schedule for the job" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"SCHEDULE" ~doc)

let required_command_third_arg =
  let doc = "Command for the job" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"COMMAND" ~doc)

let required_enabled_fourth_arg =
  let doc = "Enabled status for the job" in
  Arg.(required & pos 3 (some bool) None & info [] ~docv:"ENABLED" ~doc)

let jobs_create name schedule command enabled yes =
  let* schedule = Cron_parsing.parse_cron_schedule schedule in
  let new_job = { name; schedule; command; enabled } in
  print_endline (printable_job new_job);
  if
    yes
    || Option.value
         (Input.user_select_boolean "Confirm job creation (y/n): ")
         ~default:false
  then (
    let* () = add_job new_job in
    print_endline "Job created successfully";
    Ok ())
  else Error (`Msg (Input.yellow "skipping creation"))

let jobs_create_cmd =
  let info = Cmd.info "create" ~doc:"Create a job" in
  Cmd.v info
    Term.(
      term_result
        (const jobs_create $ required_name_first_arg
       $ required_schedule_second_arg $ required_command_third_arg
       $ required_enabled_fourth_arg $ yes_flag))

let jobs_cmd_group =
  let info = Cmd.info "jobs" ~doc:"Job-related commands" in
  Cmd.group info
    [
      jobs_list_cmd;
      jobs_create_cmd;
      jobs_get_cmd;
      jobs_delete_cmd;
      jobs_enable_cmd;
      jobs_disable_cmd;
      jobs_put_cmd;
      jobs_update_cmd;
    ]

(* TODO: perhaps some warning that this will overwrite existing ocron config? *)
(* TODO: should this even overwrite existing config? *)
(* TODO: how should specifying enabled/disabled be handled here? *)
(* TODO: option to remove import entries from crontab? *)
(* TODO: in general this import needs a lot of work *)
(* let import () =
  let* crontab_entries = Cron_migration.get_current_user_crontab_entries () in
  match crontab_entries with
  | [] ->
      print_endline "No existing jobs found";
      Ok ()
  | crontab_entries ->
      print_endline
        (if List.length crontab_entries = 1 then
           "1 job was found in the current user's crontab; indicate whether \
            you would like to import it:"
         else
           Printf.sprintf
             "%d jobs were found in current user's crontab, select which to \
              import:"
             (List.length crontab_entries));

      let entries_to_import =
        Input.user_select_many_from (Array.of_list crontab_entries)
          (fun entry -> entry)
      in
      print_newline ();
      if List.length entries_to_import = 0 then
        Printf.printf "Not importing any jobs from existing crontab\n";

      let* entries =
        map_result Cron_parsing.parse_crontab_entry entries_to_import
      in

      let import_job i (entry : Cron_parsing.crontab_entry) =
        Printf.printf "Job %d of %d\n" (i + 1) (List.length entries);

        let schedule_string =
          Cron_parsing.cron_schedule_string entry.schedule
        in
        Printf.printf "→ Importing: %s\n"
          (Input.bright_blue
             (Printf.sprintf "%s %s" schedule_string entry.command));

        Printf.printf "  Enter a name: ";
        let name = read_line () in

        Input.clear_lines 4;
        let new_job =
          {
            name;
            schedule = entry.schedule;
            command = entry.command;
            enabled = true;
          }
        in
        (* TODO: output needs to be much improved *)
        match add_job new_job with
        | Ok _ ->
            print_endline "success";
            true
        | Error err ->
            print_endline err;
            false
      in
      let statuses = List.mapi import_job entries in
      if List.for_all (fun status -> status) statuses then Ok ()
      else Error (`Msg "not all jobs were imported successfully")

let import_cmd =
  let info = Cmd.info "import" ~doc:"Import crontab entries into ocron" in
  Cmd.v info Term.(term_result (const import $ const ())) *)

let init () =
  if config_file_exists () then
    Error (`Msg (Input.red "config file already exists"))
  else
    let* () = put_jobs [] in
    print_endline (Input.green "Created config file ✓");
    Ok ()

let init_cmd =
  let info = Cmd.info "init" ~doc:"Initialize ocron config" in
  Cmd.v info Term.(term_result (const init $ const ()))

let () =
  let info = Cmd.info "ocron" ~doc:"CLI for ocron" in
  let main_cmd = Cmd.group info [ jobs_cmd_group; init_cmd ] in
  exit (Cmd.eval main_cmd)
