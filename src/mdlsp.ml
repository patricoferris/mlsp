open Eio.Std

module Doc = struct
  type t = { md : Cmarkit.Doc.t; doc : Lsp.Text_document.t }

  let of_file path =
    let uri = Eio.Path.native_exn path |> Lsp.Uri.of_string in
    let text = Eio.Path.load path in
    let md = Cmarkit.Doc.of_string text in
    let textDocument =
      Lsp.Types.TextDocumentItem.create ~languageId:"md" ~uri ~text ~version:1
    in
    let doc_params = Lsp.Types.DidOpenTextDocumentParams.create ~textDocument in
    let doc = Lsp.Text_document.make ~position_encoding:`UTF8 doc_params in
    { md; doc }
end

module Server = struct
  let loop ~sw flow f =
    let buf = Eio.Buf_read.of_flow ~max_size:max_int flow in
    let rec loop () =
      match Io.read buf with
      | None -> loop ()
      | Some pkt -> (
          match f pkt with
          | None -> loop ()
          | Some res ->
              Io.write flow res;
              loop ())
    in
    Fiber.fork ~sw loop

  let run ~sw (conn : _ Eio.Flow.two_way) handler = loop ~sw conn handler
end

let on_notification f = function
  | Jsonrpc.Packet.Notification n -> f ?params:n.params n.method_
  | _ -> None

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
