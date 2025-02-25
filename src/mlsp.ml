module T = Lsp.Types

let src = Logs.Src.create "mlsp.server" ~doc:"LSP server"

module Log = (val Logs.src_log src : Logs.LOG)

type ('req, 'res) handle = 'req Type.Id.t * 'res Type.Id.t

type info = {
  method_ : string;
  id : Jsonrpc.Id.t;
}

type request =
  | Request :
      ('request, 'response) handle
      * 'request
      * 'response Lsp.Client_request.t
      * info
      -> request

let method_ (Request (_, _, _, info)) = info.method_
let id (Request (_, _, _, info)) = info.id

let pp_request ppf t =
  Fmt.pf ppf "id: %i, method: %s" (Jsonrpc.Id.hash (id t)) (method_ t)

type response = Jsonrpc.Response.t
type handler = request -> response option
type middleware = handler -> handler

let logger : middleware =
 fun next_handler req ->
  Log.info (fun f -> f ~header:"mdlsp" "%a" pp_request req);
  next_handler req

let noop = fun _ -> None

type route = Route : ('req, 'res) handle * ('req -> 'res) -> route

let codelens_params_kind : T.CodeLensParams.t Type.Id.t = Type.Id.make ()
let codelens_kind : T.CodeLens.t list Type.Id.t = Type.Id.make ()
let codelens f = Route ((codelens_params_kind, codelens_kind), f)
let code_action_params_kind : T.CodeActionParams.t Type.Id.t = Type.Id.make ()
let code_action_result_kind : T.CodeActionResult.t Type.Id.t = Type.Id.make ()
let code_action f = Route ((code_action_params_kind, code_action_result_kind), f)
let hover_params_kind : T.HoverParams.t Type.Id.t = Type.Id.make ()
let hover_kind : T.Hover.t option Type.Id.t = Type.Id.make ()
let hover f = Route ((hover_params_kind, hover_kind), f)
let initialize_params_kind : T.InitializeParams.t Type.Id.t = Type.Id.make ()
let initialize_result_kind : T.InitializeResult.t Type.Id.t = Type.Id.make ()
let initialize f = Route ((initialize_params_kind, initialize_result_kind), f)

let map_requests (type a) : info * a Lsp.Client_request.t -> request option =
  function
  | info, (Lsp.Client_request.TextDocumentCodeLens params as r) ->
    Some (Request ((codelens_params_kind, codelens_kind), params, r, info))
  | info, (Lsp.Client_request.TextDocumentHover params as r) ->
    Some (Request ((hover_params_kind, hover_kind), params, r, info))
  | info, (Lsp.Client_request.Initialize params as r) ->
    Some
      (Request
         ((initialize_params_kind, initialize_result_kind), params, r, info))
  | info, (Lsp.Client_request.CodeAction params as r) ->
    Some
      (Request
         ((code_action_params_kind, code_action_result_kind), params, r, info))
  | _ -> None

let routes (routes : route list) : handler =
 fun (Request ((reqk, resk), req, full_req, info) as r) ->
  let rec loop = function
    | [] -> noop r
    | Route ((reqk', resk'), h) :: rest -> (
      match
        (Type.Id.provably_equal reqk reqk', Type.Id.provably_equal resk resk')
      with
      | Some Type.Equal, Some Type.Equal ->
        let v = h req in
        Logs.info (fun f ->
            f "Responding to %i: %s" (Jsonrpc.Id.hash info.id) info.method_);
        let r = Lsp.Client_request.yojson_of_result full_req v in
        Some (Jsonrpc.Response.ok info.id r)
      | _ ->
        Log.info (fun f -> f "No handler installed for %s" info.method_);
        loop rest)
  in
  loop routes

module Server = struct
  let loop reader writer nf (f : handler) =
    let rec loop () : [`Continue] =
      match Io.read reader with
      | None -> loop ()
      | Some pkt -> (
        match pkt with
        | Jsonrpc.Packet.Request request -> (
          match Lsp.Client_request.of_jsonrpc request with
          | Ok (Lsp.Client_request.E req) -> (
            let info = {method_ = request.method_; id = request.id} in
            match map_requests (info, req) with
            | Some r -> (
              match f r with
              | Some response ->
                Io.write writer (Jsonrpc.Packet.Response response);
                loop ()
              | None -> loop ())
            | None -> loop ())
          | Error e -> failwith e)
        | Jsonrpc.Packet.Notification notif -> (
          match Lsp.Client_notification.of_jsonrpc notif with
          | Ok notif ->
            Option.iter (fun f -> f notif) nf;
            loop ()
          | Error e -> failwith e)
        | _ -> loop ())
    in
    let run () =
      let `Continue = loop () in
      ()
    in
    run ()

  let run ?on_notification ~init reader writer handler =
    let reader = Io.make_reader reader in
    let extended_handler req =
      match routes [initialize init] req with
      | Some res -> Some res
      | None -> handler req
    in
    loop reader writer on_notification extended_handler
end

let run = Server.run

(* Useful helpers *)

let capabilities ?(hover = false) ?(call_hierarchy = false)
    ?(code_action = false) ?codelens ?(color = false) ?completion
    ?(declaration = false) ?(definition = false) ?diagnostic
    ?(document_formatting = false) ?(document_highlight = false) ?document_link
    ?document_on_type_formatting ?(document_range_formatting = false)
    ?(document_symbol = false) ?execute_command ?experimental
    ?(folding_range = false) ?(implementation = false) ?(inlay_hint = false)
    ?(inline_completion = false) ?(inline_value = false)
    ?(linked_editing_range = false) ?(moniker = false) ?notebook_document_sync
    ?position_encoding ?(references = false) ?(rename = false)
    ?(selection_range = false) ?semantic_tokens ?signature_help ?text_document
    ?text_document_sync ?(type_definition = false) ?(type_hierarchy = false)
    ?workspace ?(workspace_symbol = false) () =
  T.ServerCapabilities.create ~hoverProvider:(`Bool hover)
    ~callHierarchyProvider:(`Bool call_hierarchy)
    ~codeActionProvider:(`Bool code_action) ?codeLensProvider:codelens
    ~colorProvider:(`Bool color) ?completionProvider:completion
    ~declarationProvider:(`Bool declaration)
    ~definitionProvider:(`Bool definition) ?diagnosticProvider:diagnostic
    ~documentFormattingProvider:(`Bool document_formatting)
    ~documentHighlightProvider:(`Bool document_highlight)
    ?documentLinkProvider:document_link
    ?documentOnTypeFormattingProvider:document_on_type_formatting
    ~documentRangeFormattingProvider:(`Bool document_range_formatting)
    ~documentSymbolProvider:(`Bool document_symbol)
    ?executeCommandProvider:execute_command ?experimental
    ~foldingRangeProvider:(`Bool folding_range)
    ~implementationProvider:(`Bool implementation)
    ~inlayHintProvider:(`Bool inlay_hint)
    ~inlineCompletionProvider:(`Bool inline_completion)
    ~inlineValueProvider:(`Bool inline_value)
    ~linkedEditingRangeProvider:(`Bool linked_editing_range)
    ~monikerProvider:(`Bool moniker)
    ?notebookDocumentSync:notebook_document_sync
    ?positionEncoding:position_encoding ~referencesProvider:(`Bool references)
    ~renameProvider:(`Bool rename)
    ~selectionRangeProvider:(`Bool selection_range)
    ?semanticTokensProvider:semantic_tokens
    ?signatureHelpProvider:signature_help ?textDocument:text_document
    ?textDocumentSync:text_document_sync
    ~typeDefinitionProvider:(`Bool type_definition)
    ~typeHierarchyProvider:(`Bool type_hierarchy) ?workspace
    ~workspaceSymbolProvider:(`Bool workspace_symbol) ()

module Markup = struct
  let str fmt =
    Format.ksprintf
      (fun s -> T.MarkupContent.create ~kind:Markdown ~value:s)
      fmt
end

module Private = struct
  module Io = Io
end
