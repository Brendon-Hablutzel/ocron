open Lsp
open Lsp.Types
open Ocron.Crontab_parsing
open Common
open Lwt.Infix

let semantic_token_type_to_string token_type =
  match SemanticTokenTypes.yojson_of_t token_type with
  | `String s -> s
  | _ -> failwith "SemanticTokenTypes.yojson_of_t did not return a string"

let semantic_token_legend =
  Array.of_list
    [
      SemanticTokenTypes.Variable;
      SemanticTokenTypes.Function;
      SemanticTokenTypes.Comment;
    ]

let semantic_token_type_to_index token_type =
  let maybe_index =
    Array.find_index (fun x -> x = token_type) semantic_token_legend
  in
  (* TODO: customize the error? *)
  Option.get maybe_index

type semantic_token = {
  delta_line : int;
  delta_start_char : int;
  length : int;
  token_type : SemanticTokenTypes.t;
}

let semantic_token_to_integer_list token =
  let semantic_token_integer = semantic_token_type_to_index token.token_type in
  [
    token.delta_line;
    token.delta_start_char;
    token.length;
    semantic_token_integer;
    0;
  ]

let cron_tokens_to_semantic_tokens cron_tokens =
  let _, _, backwards_tokens =
    List.fold_left
      (fun (last_line_index, last_char_index, acc_list) token ->
        let line_index, char_index, length, semantic_token_type =
          match token with
          | Comment { line_index; content } ->
              (line_index, 0, String.length content, SemanticTokenTypes.Comment)
          | Term
              { line_index; char_index; term_type = _; term_value = _; content }
            ->
              ( line_index,
                char_index,
                String.length content,
                SemanticTokenTypes.Variable )
          | Command { line_index; char_index; content } ->
              ( line_index,
                char_index,
                String.length content,
                SemanticTokenTypes.Function )
        in
        let delta_line = line_index - last_line_index in
        let delta_char =
          if delta_line = 0 then char_index - last_char_index else char_index
        in
        ( line_index,
          char_index,
          {
            delta_line;
            delta_start_char = delta_char;
            length;
            token_type = semantic_token_type;
          }
          :: acc_list ))
      (0, 0, []) cron_tokens
  in
  let semantic_tokens = List.rev backwards_tokens in
  let integer_arr_tokens =
    List.map semantic_token_to_integer_list semantic_tokens
  in
  List.iter
    (fun integer_arr ->
      List.iter
        (fun i -> output_string stderr (Printf.sprintf "%d " i))
        integer_arr;
      output_char stderr '\n')
    integer_arr_tokens;
  List.flatten integer_arr_tokens

let on_text_document_did_open (state : Common.state)
    (params : DidOpenTextDocumentParams.t) =
  let uri = Uri.to_string params.textDocument.uri in
  let text = params.textDocument.text in
  let tokens = tokenize_file text in

  log_stderr (Printf.sprintf "Opened document: %s" uri);
  log_stderr (Printf.sprintf "Text content: %s" text);

  Lwt_mutex.with_lock state.mutex (fun () ->
      UriMap.replace state.open_files uri tokens;
      Lwt.return_unit)

let on_text_document_did_change (state : state)
    (params : DidChangeTextDocumentParams.t) =
  let uri = Uri.to_string params.textDocument.uri in
  let content_changes = params.contentChanges in
  let text =
    String.concat ""
      (List.map
         (fun (change : TextDocumentContentChangeEvent.t) -> change.text)
         content_changes)
  in
  let tokens = tokenize_file text in

  log_stderr (Printf.sprintf "Document changed: %s" uri);
  log_stderr (Printf.sprintf "Text content: %s" text);

  Lwt_mutex.with_lock state.mutex (fun () ->
      UriMap.replace state.open_files uri tokens;
      Lwt.return_unit)

let on_text_document_did_close (state : state)
    (params : DidCloseTextDocumentParams.t) =
  let uri = Uri.to_string params.textDocument.uri in

  log_stderr (Printf.sprintf "Document closed: %s" uri);

  Lwt_mutex.with_lock state.mutex (fun () ->
      UriMap.remove state.open_files uri;
      Lwt.return_unit)

(* TODO: line_tokens will be used for full schedule interpretation *)
let get_hovered_token_text token _line_tokens =
  match token with
  | Term { term_type; term_value = Ok term_value; _ } ->
      let readable_type = readable_cron_term_type term_type in
      let readable_value = readable_cron_term_value term_value in
      Some (Printf.sprintf "%s term: %s" readable_type readable_value)
  | Term { term_value = Error _; _ } -> None
  | Command { content; _ } ->
      (* TODO: provide full human readable schedule interpretation *)
      Some (content ^ " will be run on the given schedule")
  | _ -> None

(* TODO: should this take line_tokens or arbitrary tokens? *)
let get_hovered_token hovered_char_index line_tokens =
  List.find_opt
    (fun x ->
      match x with
      | Comment _ -> true
      | Term { char_index; content; _ } ->
          hovered_char_index >= char_index
          && hovered_char_index < char_index + String.length content
      | Command { char_index; content; _ } ->
          hovered_char_index >= char_index
          && hovered_char_index < char_index + String.length content)
    line_tokens

let on_hover (state : state) (params : HoverParams.t) : Hover.t Lwt.t =
  let uri = Uri.to_string params.textDocument.uri in
  Lwt_mutex.with_lock state.mutex (fun () ->
      Lwt.return (UriMap.find_opt state.open_files uri))
  >>= fun file_tokens ->
  let res =
    match file_tokens with
    | Some tokens -> (
        let line_tokens = get_line_tokens params.position.line tokens in
        let hovered_token =
          get_hovered_token params.position.character line_tokens
        in
        match hovered_token with
        | Some token -> (
            let start, end_ = get_token_char_range token in
            let text = get_hovered_token_text token line_tokens in
            match text with
            | Some text ->
                (* TODO: what is language? *)
                Hover.create
                  ~contents:(`MarkedString { value = text; language = None })
                  ~range:
                    {
                      start = { line = params.position.line; character = start };
                      end_ = { line = params.position.line; character = end_ };
                    }
                  ()
            | None ->
                Hover.create
                  ~contents:(`MarkedString { value = ""; language = None })
                  ())
        | None ->
            Hover.create
              ~contents:(`MarkedString { value = ""; language = None })
              ())
    | None ->
        Hover.create
          ~contents:(`MarkedString { value = ""; language = None })
          ()
  in
  Lwt.return res

let on_semantic_tokens_full (state : state) (params : SemanticTokensParams.t) :
    SemanticTokens.t Lwt.t =
  let uri = Uri.to_string params.textDocument.uri in

  log_stderr (Printf.sprintf "Received semantic tokens full request: %s" uri);

  Lwt_mutex.with_lock state.mutex (fun () ->
      Lwt.return (UriMap.find_opt state.open_files uri))
  >>= fun file_tokens ->
  let file_tokens =
    match file_tokens with
    | Some tokens -> tokens
    | None -> failwith (Printf.sprintf "%s is not a tracked open file" uri)
  in

  List.iter
    (function
      | Comment { content; _ } -> log_stderr content
      | Command { content; _ } -> log_stderr content
      | Term { content; _ } -> log_stderr content)
    file_tokens;

  let semantic_tokens =
    Array.of_list (cron_tokens_to_semantic_tokens file_tokens)
  in
  Lwt.return (SemanticTokens.create ~data:semantic_tokens ())
