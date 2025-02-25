open Bytesrw
(** {1 Mlsp}

    A small (micro) LSP toolkit for building servers in OCaml. *)

module T = Lsp.Types

type request
(** A client request *)

val method_ : request -> string
(** [method_ req] returns the raw method string for the client request *)

val id : request -> Jsonrpc.Id.t
(** The unique ID for the client request *)

type response
(** A server response *)

type handler = request -> response option
(** A handler takes a request from an LSP client and, optionally, returns a
    response.

    If [None] is returned then we treat the request as ignored. *)

type middleware = handler -> handler
(** Middleware sits inbetween handlers, usually doing some side-effecting
    action.

    See {! logger} for an example middleware. *)

val logger : middleware
(** The logger will log all incoming requests *)

(** {2 Routes}

    Routes are the main way you can customise your LSP server. They allow you to
    define what your server does in response to specific incoming client
    requests.

    Your server {b must} implement the [initialize] route in order for you
    server to complete a handshake with a client. The {! capabilities} function
    may be of use. *)

type route

val routes : route list -> handler
(** Make a handler from a list of routes. The routes will be tried in the order
    they are given. *)

val hover : (T.HoverParams.t -> T.Hover.t option) -> route
(** Creates a hover route. *)

val code_action : (T.CodeActionParams.t -> T.CodeActionResult.t) -> route
(** Creates a code action route. *)

val codelens : (T.CodeLensParams.t -> T.CodeLens.t list) -> route
(** Creates a codelens route. *)

(** {2 Running the server} *)

val run :
  ?on_notification:(Lsp.Client_notification.t -> unit) ->
  init:(T.InitializeParams.t -> T.InitializeResult.t) ->
  Bytes.Reader.t ->
  Bytes.Writer.t ->
  handler ->
  unit
(** Run the server using a {! Bytes.Reader.t} and a {! Bytes.Writer.t}.

    @param init
      A special handler that is required for any server. Mostly [init] will let
      the client know what capabilities the server can handle. *)

(** {2 Markup} *)

module Markup : sig
  val str : ('a, unit, string, T.MarkupContent.t) format4 -> 'a
  (** [str] creates a {! T.MarkupContent.t} from a format string. *)
end

(** {2 Helpers} *)

val capabilities :
  ?hover:bool ->
  ?call_hierarchy:bool ->
  ?code_action:bool ->
  ?codelens:T.CodeLensOptions.t ->
  ?color:bool ->
  ?completion:T.CompletionOptions.t ->
  ?declaration:bool ->
  ?definition:bool ->
  ?diagnostic:
    [ `DiagnosticOptions of T.DiagnosticOptions.t
    | `DiagnosticRegistrationOptions of T.DiagnosticRegistrationOptions.t ] ->
  ?document_formatting:bool ->
  ?document_highlight:bool ->
  ?document_link:T.DocumentLinkOptions.t ->
  ?document_on_type_formatting:T.DocumentOnTypeFormattingOptions.t ->
  ?document_range_formatting:bool ->
  ?document_symbol:bool ->
  ?execute_command:T.ExecuteCommandOptions.t ->
  ?experimental:Yojson.Safe.t ->
  ?folding_range:bool ->
  ?implementation:bool ->
  ?inlay_hint:bool ->
  ?inline_completion:bool ->
  ?inline_value:bool ->
  ?linked_editing_range:bool ->
  ?moniker:bool ->
  ?notebook_document_sync:
    [ `NotebookDocumentSyncOptions of unit
    | `NotebookDocumentSyncRegistrationOptions of unit ] ->
  ?position_encoding:T.PositionEncodingKind.t ->
  ?references:bool ->
  ?rename:bool ->
  ?selection_range:bool ->
  ?semantic_tokens:
    [ `SemanticTokensOptions of T.SemanticTokensOptions.t
    | `SemanticTokensRegistrationOptions of
      T.SemanticTokensRegistrationOptions.t ] ->
  ?signature_help:T.SignatureHelpOptions.t ->
  ?text_document:T.ServerCapabilities.textDocument ->
  ?text_document_sync:
    [ `TextDocumentSyncKind of T.TextDocumentSyncKind.t
    | `TextDocumentSyncOptions of T.TextDocumentSyncOptions.t ] ->
  ?type_definition:bool ->
  ?type_hierarchy:bool ->
  ?workspace:T.ServerCapabilities.workspace ->
  ?workspace_symbol:bool ->
  unit ->
  T.ServerCapabilities.t

module Private : sig
  module Io = Io
end
