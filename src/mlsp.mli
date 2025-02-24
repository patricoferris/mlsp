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
    server to complete a handshake with a client. *)

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
  sw:Eio.Switch.t ->
  init:(T.InitializeParams.t -> T.InitializeResult.t) ->
  _ Eio.Flow.two_way ->
  handler ->
  unit
(** Run the server.

    @param init
      A special handler that is required for any server. Mostly [init] will let
      the client know what capabilities the server can handle. *)

val conn_from_src_and_sink :
  _ Eio.Flow.source -> _ Eio.Flow.sink -> Eio.Flow.two_way_ty Eio.Flow.two_way
(** A helper for creating a {! Eio.Flow.two_way} from [stdin] and [stdout]. *)

(** {2 Markup} *)

module Markup : sig
  val str : ('a, unit, string, T.MarkupContent.t) format4 -> 'a
  (** [str] creates a {! T.MarkupContent.t} from a format string. *)
end
