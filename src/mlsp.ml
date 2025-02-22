open Eio.Std

let src = Logs.Src.create "mlsp.server" ~doc:"LSP server"

module Log = (val Logs.src_log src : Logs.LOG)

let exn = function
  | Ok v -> v
  | Error s -> failwith s

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

let codelens_params_kind : Lsp.Types.CodeLensParams.t Type.Id.t =
  Type.Id.make ()

let codelens_kind : Lsp.Types.CodeLens.t list Type.Id.t = Type.Id.make ()
let codelens f = Route ((codelens_params_kind, codelens_kind), f)

let code_action_params_kind : Lsp.Types.CodeActionParams.t Type.Id.t =
  Type.Id.make ()

let code_action_result_kind : Lsp.Types.CodeActionResult.t Type.Id.t =
  Type.Id.make ()

let code_action f = Route ((code_action_params_kind, code_action_result_kind), f)
let hover_params_kind : Lsp.Types.HoverParams.t Type.Id.t = Type.Id.make ()
let hover_kind : Lsp.Types.Hover.t option Type.Id.t = Type.Id.make ()
let text_document_hover f = Route ((hover_params_kind, hover_kind), f)

let initialize_params_kind : Lsp.Types.InitializeParams.t Type.Id.t =
  Type.Id.make ()

let initialize_result_kind : Lsp.Types.InitializeResult.t Type.Id.t =
  Type.Id.make ()

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

let on_notification f = function
  | Jsonrpc.Packet.Notification n ->
    let decode = Lsp.Client_notification.of_jsonrpc n |> exn in
    let s = f decode in
    Option.map Lsp.Server_notification.to_jsonrpc s
    |> Option.map (fun n -> Jsonrpc.Packet.Notification n)
  | _ -> None

module Server = struct
  let loop ~sw flow nf (f : handler) =
    let buf = Eio.Buf_read.of_flow ~max_size:max_int flow in
    let rec loop () : [`Continue] =
      Logs.info (fun f -> f "Looping");
      match Io.read buf with
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
                Io.write flow (Jsonrpc.Packet.Response response);
                loop ()
              | None -> loop ())
            | None ->
              Eio.traceln "Skipping non-client requests";
              loop ())
          | Error e -> failwith e)
        | Jsonrpc.Packet.Notification notif -> (
          match Lsp.Client_notification.of_jsonrpc notif with
          | Ok notif ->
            nf notif;
            loop ()
          | Error e -> failwith e)
        | _ ->
          Eio.traceln "No packet!";
          loop ())
    in
    let run () =
      let `Continue = loop () in
      ()
    in
    Fiber.fork ~sw run

  let run ~sw (conn : _ Eio.Flow.two_way) handler = loop ~sw conn handler
end

let two_way_from_source_and_sink (source : _ Eio.Flow.source)
    (sink : _ Eio.Flow.sink) : Eio.Flow.two_way_ty r =
  let module X = struct
    type t = {
      source : Eio.Flow.source_ty Eio.Flow.source;
      sink : Eio.Flow.sink_ty Eio.Flow.sink;
    }

    let single_read t = Eio.Flow.single_read t.source
    let shutdown _ _ = ()
    let single_write t = Eio.Flow.single_write t.sink
    let copy t ~src = Eio.Flow.copy src t.sink
    let read_methods = []
  end in
  let handler =
    Eio.Resource.handler
      [
        Eio.Resource.H (Eio.Flow.Pi.Source, (module X));
        Eio.Resource.H (Eio.Flow.Pi.Sink, (module X));
        Eio.Resource.H (Eio.Flow.Pi.Shutdown, (module X));
      ]
  in
  Eio.Resource.T
    ( X.
        {
          source :> Eio.Flow.source_ty Eio.Flow.source;
          sink :> Eio.Flow.sink_ty Eio.Flow.sink;
        },
      handler )
