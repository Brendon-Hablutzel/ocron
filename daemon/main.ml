(* TODO: job execution daemon *)

let () = print_endline "daemon"

(* TODO: working idea:
- run daemon routine every 30 seconds
- on startup, load all jobs from config and calculate the time of their next invocation
- create a min heap with timestamp as the key
- every minute, complete the following routine:
  - check if config file has changed; if so, adjust min heap as necessary
  - check the top element of the min heap: if its timestamp is less than or equal to the current 
    time, pop it, calculate its next invocation time and push that to the heap, then execute it
    in a child process
  - repeat previous step until top element of min heap has timestamp greater than current
*)

(* TODO: how to address concurrency issues for very frequent jobs? *)
(* TODO: how to efficiently update heap with new jobs? heap may not even be the best fit *)

(* optimize for computations that occur at execution time, since restarts and config changes
should be comparatively rare *)
