open Eio.Std
module T = Lsp.Types

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let on_notification = function
  | Lsp.Client_notification.Initialized ->
    Eio.traceln "Successfully initialized"
  | _ -> Eio.traceln "Received notification"

let main ~sw conn =
  let on_hover (p : T.HoverParams.t) : T.Hover.t option =
    let s =
      Mlsp.Markup.str "It **seems** you are hovering at `%i:%i`" p.position.line
        p.position.character
    in
    Some (T.Hover.create ~contents:(`MarkupContent s) ())
  in
  let init (_ : T.InitializeParams.t) =
    let capabilities =
      T.ServerCapabilities.create ~hoverProvider:(`Bool true) ()
    in
    T.InitializeResult.create ~capabilities ()
  in
  let handler = Mlsp.logger @@ Mlsp.routes [Mlsp.hover on_hover] in
  Mlsp.run ~sw ~init ~on_notification conn handler

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let two_way_std = Mlsp.conn_from_src_and_sink env#stdin env#stdout in
  main ~sw two_way_std
