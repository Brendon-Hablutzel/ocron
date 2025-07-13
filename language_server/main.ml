open Lsp
open Lsp.Types
open Jsonrpc
open Lwt.Infix
open Ocron.Crontab_parsing
open Text_doc_service
open Common

let diagnostics_of_tokens (tokens : cron_token list) : Diagnostic.t list =
  List.filter_map
    (function
      | Term
          { term_type; term_value = Error err; content; line_index; char_index }
        ->
          let err =
            Printf.sprintf "%s term: %s" (readable_cron_term_type term_type) err
          in

          Some
            (Diagnostic.create
               ~range:
                 {
                   start = { line = line_index; character = char_index };
                   end_ =
                     {
                       line = line_index;
                       character = char_index + String.length content;
                     };
                 }
               ~severity:DiagnosticSeverity.Error ~message:(`String err) ())
      | _ -> None)
    tokens

let on_initialize (_params : InitializeParams.t) : InitializeResult.t Lwt.t =
  let semantic_tokens_legend =
    SemanticTokensLegend.create ~tokenModifiers:[]
      ~tokenTypes:
        (Array.to_list
           (Array.map
              (fun token_type -> semantic_token_type_to_string token_type)
              semantic_token_legend))
  in

  let semantic_tokens_provider =
    SemanticTokensOptions.create ~legend:semantic_tokens_legend ~range:false
      ~full:(`Bool true) ()
  in
  let capabilities =
    ServerCapabilities.create
      ~textDocumentSync:(`TextDocumentSyncKind TextDocumentSyncKind.Full)
      ~hoverProvider:(`Bool true)
      ~semanticTokensProvider:(`SemanticTokensOptions semantic_tokens_provider)
      ()
  in
  let server_info : InitializeResult.serverInfo =
    { name = "crontab-ls"; version = Some "0.1" }
  in
  let result : InitializeResult.t =
    { capabilities; serverInfo = Some server_info }
  in
  Lwt.return result

let read_headers ic =
  let rec loop headers =
    Lwt_io.read_line_opt ic >>= function
    | Some "" -> Lwt.return headers
    | Some line -> (
        match String.split_on_char ':' line with
        | [ key; value ] ->
            loop ((String.lowercase_ascii key, String.trim value) :: headers)
        | _ -> Lwt.fail_with ("Invalid header: " ^ line))
    | None -> Lwt.fail_with "Unexpected end of headers"
  in
  loop []

let rec read_exactly ic n =
  if n = 0 then Lwt.return ""
  else
    Lwt_io.read ~count:n ic >>= fun chunk ->
    let len = String.length chunk in
    if len = 0 then Lwt.fail_with "Unexpected EOF while reading body"
    else read_exactly ic (n - len) >>= fun rest -> Lwt.return (chunk ^ rest)

let read_content ic =
  read_headers ic >>= fun headers ->
  match List.assoc_opt "content-length" headers with
  | Some len_str ->
      let len = int_of_string len_str in
      read_exactly ic len
  | None -> Lwt.fail_with "Missing Content-Length header"

let write_response oc (packet : Jsonrpc.Packet.t) =
  let json = Jsonrpc.Packet.yojson_of_t packet |> Yojson.Safe.to_string in
  let content =
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json
  in
  Lwt_io.write oc content

let get_params_json_or_throw (params : Structured.t option) =
  match params with
  | Some structured -> Jsonrpc.Structured.yojson_of_t structured
  | None -> failwith "Missing params"

let send_diagnostics oc uri diagnostics =
  let params = PublishDiagnosticsParams.create ~uri ~diagnostics () in
  let notification =
    Jsonrpc.Notification.create ~method_:"textDocument/publishDiagnostics"
      ~params:
        (Structured.t_of_yojson (PublishDiagnosticsParams.yojson_of_t params))
      ()
  in
  let packet = Jsonrpc.Packet.Notification notification in
  write_response oc packet

let generate_and_send_diagnostics (state : state) oc (uri : Uri.t) =
  Lwt_mutex.with_lock state.mutex (fun () ->
      Lwt.return (UriMap.find state.open_files (Uri.to_string uri)))
  >>= fun tokens ->
  let diagnostics = diagnostics_of_tokens tokens in
  send_diagnostics oc uri diagnostics

let rec message_loop ic oc (state : state) =
  read_content ic >>= fun content_str ->
  let json = Yojson.Safe.from_string content_str in
  let packet = Jsonrpc.Packet.t_of_yojson json in
  match packet with
  | Packet.Request req -> (
      match req.method_ with
      | "initialize" ->
          let json = get_params_json_or_throw req.params in
          let params = InitializeParams.t_of_yojson json in
          on_initialize params >>= fun result ->
          let response =
            Jsonrpc.Response.ok req.id (InitializeResult.yojson_of_t result)
          in
          (* TODO: replace all instances of >>= with let%lwt (see docs on >>=) *)
          write_response oc (Packet.Response response) >>= fun () ->
          message_loop ic oc state
      | "textDocument/hover" ->
          let json = get_params_json_or_throw req.params in
          let params = HoverParams.t_of_yojson json in
          on_hover state params >>= fun result ->
          let response =
            Jsonrpc.Response.ok req.id (Hover.yojson_of_t result)
          in
          write_response oc (Packet.Response response) >>= fun () ->
          message_loop ic oc state
      | "textDocument/semanticTokens/full" ->
          let json = get_params_json_or_throw req.params in
          let params = SemanticTokensParams.t_of_yojson json in
          on_semantic_tokens_full state params >>= fun result ->
          let response =
            Jsonrpc.Response.ok req.id (SemanticTokens.yojson_of_t result)
          in
          write_response oc (Packet.Response response) >>= fun () ->
          message_loop ic oc state
      | "shutdown" ->
          let response = Jsonrpc.Response.ok req.id `Null in
          write_response oc (Packet.Response response) >>= fun () ->
          message_loop ic oc state
      | _ ->
          log_stderr ("Unhandled request: " ^ req.method_);
          let err =
            Jsonrpc.Response.Error.make
              ~code:Jsonrpc.Response.Error.Code.MethodNotFound
              ~message:"Unhandled method" ()
          in
          let resp = Jsonrpc.Response.error req.id err in
          write_response oc (Packet.Response resp) >>= fun () ->
          message_loop ic oc state)
  | Packet.Notification notif -> (
      match notif.method_ with
      | "textDocument/didOpen" ->
          let json = get_params_json_or_throw notif.params in
          let params = DidOpenTextDocumentParams.t_of_yojson json in
          on_text_document_did_open state params >>= fun () ->
          generate_and_send_diagnostics state oc params.textDocument.uri
          >>= fun () -> message_loop ic oc state
      | "textDocument/didChange" ->
          let json = get_params_json_or_throw notif.params in
          let params = DidChangeTextDocumentParams.t_of_yojson json in
          on_text_document_did_change state params >>= fun () ->
          generate_and_send_diagnostics state oc params.textDocument.uri
          >>= fun () -> message_loop ic oc state
      | "textDocument/didClose" ->
          let json = get_params_json_or_throw notif.params in
          let params = DidCloseTextDocumentParams.t_of_yojson json in
          on_text_document_did_close state params >>= fun () ->
          message_loop ic oc state
      | "exit" -> exit 0
      | _ ->
          log_stderr ("Unhandled notification: " ^ notif.method_);
          message_loop ic oc state)
  | Packet.Response _ ->
      log_stderr "Unexpected response packet";
      message_loop ic oc state
  | Packet.Batch_call _ | Packet.Batch_response _ ->
      log_stderr "Batch requests not supported";
      message_loop ic oc state

let () =
  let ic = Lwt_io.stdin in
  let oc = Lwt_io.stdout in
  Lwt_main.run
    (message_loop ic oc
       { open_files = UriMap.create 16; mutex = Lwt_mutex.create () })
