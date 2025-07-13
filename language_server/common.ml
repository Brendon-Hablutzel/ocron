open Ocron.Crontab_parsing

module UriMap = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

type state = { open_files : cron_token list UriMap.t; mutex : Lwt_mutex.t }

let log_stderr message =
  output_string stderr (message ^ "\n");
  flush stderr
