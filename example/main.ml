open Eio.Std

let main ~sw conn =
  let handler ?params:_ method_ =
    match method_ with
    | m ->
        Eio.traceln "Notification: %s" m;
        None
  in
  let handler = Mdlsp.on_notification handler in
  Mdlsp.Server.run ~sw conn handler

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  Eio.traceln "Example Mdlsp Server";
  let two_way_std = Mdlsp.two_way_from_source_and_sink env#stdin env#stdout in
  main ~sw two_way_std
