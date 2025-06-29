open Ocron
open Ocron.Config

let ( let* ) = Lwt.bind

module StringKey = struct
  type t = string

  let compare = String.compare
end

module JobsPsqPriority = struct
  type t = float * job

  let compare (p1, _) (p2, _) = Float.compare p1 p2
end

module JobsPsq = Psq.Make (StringKey) (JobsPsqPriority)

(* for debugging *)
(* let print_jobs_psq jobs_psq =
  let l = JobsPsq.to_list jobs_psq in
  List.iter
    (fun (s, (t, _z)) ->
      print_endline s;
      print_float t;
      print_newline () (* print_endline t;
      print_endline z; *))
    l;
  () *)

let possible_job_thunk job =
 fun () ->
  let split_command = String.split_on_char ' ' job.command in
  match split_command with
  | [] -> Lwt.return_none
  | command :: args ->
      let full_command = (command, Array.of_list (command :: args)) in
      let process = Lwt_process.open_process_full full_command in

      let stdout_promise = Lwt_io.read process#stdout in
      let stderr_promise = Lwt_io.read process#stderr in
      let status_promise = process#status in

      let* stdout = stdout_promise in
      let* stderr = stderr_promise in
      let* status = status_promise in
      Lwt.return_some (stdout, stderr, status)

let handle_job_results results =
  let print_result ((stdout, stderr, status), job) =
    let status_str =
      match status with
      | Unix.WEXITED n -> "exit " ^ string_of_int n
      | Unix.WSIGNALED n -> "signaled " ^ string_of_int n
      | Unix.WSTOPPED n -> "stopped " ^ string_of_int n
    in

    Lwt_io.printl
      (Printf.sprintf "--- %s : %s ---\nstdout:\n%s\nstderr:\n%s\nstatus: %s\n"
         job.name job.command stdout stderr status_str)
  in

  let print_all = List.map print_result results in
  Lwt.join print_all

let execute_all_jobs jobs =
  let thunks = List.map possible_job_thunk jobs in
  let* results = Lwt_list.map_p (fun thunk -> thunk ()) thunks in
  let results_with_jobs = List.combine results jobs in
  let successful_results, dropped_jobs =
    List.partition_map
      (function
        | Some result, job -> Left (result, job) | None, job -> Right job)
      results_with_jobs
  in
  let* () =
    Lwt_list.iter_p
      (fun job ->
        Lwt_io.printl
          (Printf.sprintf "Invalid command for %s: %s" job.name job.command))
      dropped_jobs
  in
  handle_job_results successful_results

let construct_psq current_time jobs =
  let psq = JobsPsq.empty in
  let rec add_jobs_to_psq remaining_jobs intermediate_psq =
    match remaining_jobs with
    | [] -> intermediate_psq
    | head :: rest ->
        let next_execution_time =
          Scheduling.calculate_next_execution_time current_time head.schedule
        in
        let new_psq =
          JobsPsq.add head.name (next_execution_time, head) intermediate_psq
        in
        add_jobs_to_psq rest new_psq
  in
  add_jobs_to_psq jobs psq

let rec pop_jobs_to_execute current_time psq =
  match JobsPsq.min psq with
  (* the next job is ready to execute *)
  | Some (_, (next_execution_time, _)) when next_execution_time <= current_time
    -> (
      match JobsPsq.pop psq with
      | Some ((_, (next_execution_time, job)), psq) ->
          let next_next_execution_time =
            Scheduling.calculate_next_execution_time current_time job.schedule
          in
          let psq = JobsPsq.add job.name (next_next_execution_time, job) psq in
          (next_execution_time, job) :: pop_jobs_to_execute current_time psq
      | None -> failwith "if Psq.min is defined, then Psq.pop should be defined"
      )
  (* no jobs ready to execute *)
  | Some _ -> []
  (* no jobs at all *)
  | None -> []

let did_config_change_since last_loaded_config =
  let last_modified = config_file_last_modified () in
  (* TODO: > or >= ? I think >= to be safe *)
  last_modified >= last_loaded_config

let rec executor_loop maybe_current_psq maybe_last_execution_time =
  let last_execution_time =
    Option.value maybe_last_execution_time ~default:0.0
  in
  let now = Unix.time () in
  let* () = Lwt_io.printl (Printf.sprintf "Executing at %f..." now) in

  let new_psq =
    match list_jobs () with
    | Ok (Some jobs) -> (
        let jobs = List.filter (fun job -> job.enabled) jobs in
        match maybe_current_psq with
        | Some current_psq ->
            if did_config_change_since last_execution_time then
              construct_psq now jobs
            else current_psq
        | None -> construct_psq now jobs)
    | Ok None -> failwith "no config found"
    | Error err -> failwith ("error getting config: " ^ err)
  in

  let ready_job_executions = pop_jobs_to_execute now new_psq in
  let jobs_to_execute = List.map (fun (_, job) -> job) ready_job_executions in

  let job_promise = execute_all_jobs jobs_to_execute in
  let sleep_duration = Scheduling.seconds_until_next_minute (Unix.time ()) in
  let sleep_promise = Lwt_unix.sleep sleep_duration in

  let* () = job_promise in
  let* () = sleep_promise in
  executor_loop (Some new_psq) (Some now)

let () = Lwt_main.run (executor_loop None None)
