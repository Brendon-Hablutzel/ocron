(* persistent job config storage via a json file in the user's home directory *)
open Cron_parsing

type job = {
  name : string;
  schedule : cron_schedule;
  command : string;
  enabled : bool;
}
[@@deriving yojson]

type job_config = { jobs : job list } [@@deriving yojson]

let jobs_config_filename = ".ocron-jobs.json"

let get_home_directory () =
  let uid = Unix.getuid () in
  let pw_entry = Unix.getpwuid uid in
  pw_entry.Unix.pw_dir

let get_jobs_config_file_path () =
  Filename.concat (get_home_directory ()) jobs_config_filename

let config_file_exists () = Sys.file_exists (get_jobs_config_file_path ())

(* helper function, should not be used directly outside this module *)
let read_jobs_config () =
  let config_file_path = get_jobs_config_file_path () in
  if not (Sys.file_exists config_file_path) then Ok None
  else
    try
      let json = Yojson.Safe.from_file config_file_path in
      match job_config_of_yojson json with
      | Ok cfg -> Ok (Some cfg)
      | Error msg -> Error ("deserialization error: " ^ msg)
    with
    | Yojson.Json_error msg -> Error ("json parse error: " ^ msg)
    | Sys_error msg -> Error ("file error: " ^ msg)

let find_duplicate_names jobs =
  let tbl = Hashtbl.create (List.length jobs) in
  List.iter
    (fun p ->
      Hashtbl.replace tbl p.name
        ((try Hashtbl.find tbl p.name with Not_found -> 0) + 1))
    jobs;
  Hashtbl.fold
    (fun name count acc -> if count > 1 then name :: acc else acc)
    tbl []

(* helper function, should not be used directly outside this module *)
let put_jobs_config config =
  match find_duplicate_names config.jobs with
  | [] -> (
      let sorted_jobs =
        List.sort (fun job_a job_b -> compare job_a.name job_b.name) config.jobs
      in
      let sorted_config = { jobs = sorted_jobs } in
      let config_file_path = get_jobs_config_file_path () in
      try
        let json = job_config_to_yojson sorted_config in
        Ok (Yojson.Safe.to_file config_file_path json)
      with
      | Sys_error msg -> Error ("file error: " ^ msg)
      | Yojson.Json_error msg -> Error ("json write error: " ^ msg))
  | duplicates ->
      Error
        (Printf.sprintf "attemped to insert duplicate job names: %s"
           (String.concat ", " duplicates))

let list_jobs () =
  Result.map (Option.map (fun config -> config.jobs)) (read_jobs_config ())

let put_jobs jobs =
  match find_duplicate_names jobs with
  | [] -> put_jobs_config { jobs }
  | _ -> Error "attempted to put jobs with the same name"

let get_job name =
  match list_jobs () with
  | Ok (Some current_jobs) ->
      Ok (List.find_opt (fun job -> job.name = name) current_jobs)
  | Ok None -> Error "no config file found"
  | Error err -> Error err

let add_job job =
  match list_jobs () with
  | Ok (Some existing_jobs) -> (
      let exising_jobs_with_name =
        List.filter
          (fun existing_job -> existing_job.name = job.name)
          existing_jobs
      in
      match exising_jobs_with_name with
      | [] -> put_jobs (job :: existing_jobs)
      | [ _ ] -> Error "job already exists, not addding"
      | _ ->
          Error
            "corrupted jobs config file, multiple jobs with name, not adding")
  | Ok None -> Error "no config file found"
  | Error err -> Error err

let delete_job name =
  match list_jobs () with
  | Ok (Some current_jobs) ->
      let job_to_remove, jobs_to_keep =
        List.partition (fun job -> job.name = name) current_jobs
      in
      if List.length job_to_remove <= 1 then
        match put_jobs jobs_to_keep with
        | Ok _ -> Ok (List.nth_opt job_to_remove 0)
        | Error err -> Error err
      else
        Error
          (Printf.sprintf
             "corrupted jobs config file, skipping removal: multiple jobs with \
              name %s"
             name)
  | Ok None -> Error "no config file found"
  | Error err -> Error err

(* TODO: there might be a better abstraction for this, since now callers have to find the job
for all its fields first *)
let put_job name new_job =
  match list_jobs () with
  | Ok (Some current_jobs) -> (
      let matching_jobs, rest =
        List.partition (fun job -> job.name = name) current_jobs
      in
      match matching_jobs with
      | [] -> Error "no job found"
      | [ current_job ] -> (
          if current_job = new_job then Ok false
          else
            let new_jobs = new_job :: rest in
            match put_jobs new_jobs with
            | Ok _ -> Ok true
            | Error err -> Error err)
      | _ ->
          Error
            (Printf.sprintf
               "corrupted jobs config file, skipping removal: multiple jobs \
                with name %s"
               name))
  | Ok None -> Error "no config file found"
  | Error err -> Error err

let set_job_enabled_status name enabled =
  match get_job name with
  | Ok (Some job) ->
      let new_job = { job with enabled } in
      put_job name new_job
  | Ok None -> Error "no job found"
  | Error err -> Error err
