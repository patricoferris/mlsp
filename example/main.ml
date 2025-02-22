open Eio.Std

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let notification_handler = function
  | Lsp.Client_notification.Initialized ->
    Eio.traceln "Successfully initialized"
  | _ -> Eio.traceln "Received notification"

let main ~sw conn =
  let on_hover (p : Lsp.Types.HoverParams.t) =
    Eio.traceln "Hovering at: (%i:%i) " p.position.line p.position.character;
    None
  in
  let handler =
    Mlsp.logger
    @@ Mlsp.routes
         [
           Mlsp.initialize (fun _ ->
               let capabilities =
                 Lsp.Types.ServerCapabilities.create ~hoverProvider:(`Bool true)
                   ()
               in
               Lsp.Types.InitializeResult.create ~capabilities ());
           Mlsp.codelens (fun _ -> []);
           Mlsp.text_document_hover on_hover;
           Mlsp.code_action (fun _ ->
               Some
                 [
                   `Command
                     (Lsp.Types.Command.create ~command:"todo" ~title:"e" ());
                 ]);
         ]
  in
  Mlsp.Server.run ~sw conn notification_handler handler

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  Eio.traceln "Example Mlsp Server";
  let two_way_std = Mlsp.two_way_from_source_and_sink env#stdin env#stdout in
  main ~sw two_way_std
