open Bytesrw
module T = Lsp.Types

let main reader writer =
  let on_hover (p : T.HoverParams.t) : T.Hover.t option =
    let s =
      Mlsp.Markup.str "It **seems** you are hovering at `%i:%i`" p.position.line
        p.position.character
    in
    Some (T.Hover.create ~contents:(`MarkupContent s) ())
  in
  let init (_ : T.InitializeParams.t) =
    let capabilities = Mlsp.capabilities ~hover:true () in
    T.InitializeResult.create ~capabilities ()
  in
  let handler = Mlsp.logger @@ Mlsp.routes [Mlsp.hover on_hover] in
  Mlsp.run ~init reader writer handler

let () =
  let reader = Bytes.Reader.of_in_channel stdin in
  let writer = Bytes.Writer.of_out_channel ~flush_slices:true stdout in
  main reader writer
