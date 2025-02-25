(* ISC License

Copyright (X) 2018-2019, the [ocaml-lsp contributors](https://github.com/ocaml/ocaml-lsp/graphs/contributors)
Copyright Patrick Ferris

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)
open Bytesrw

type reader = {
  reader : Bytes.Reader.t;
  buf : Buffer.t;
}

let make_reader reader = {reader; buf = Buffer.create 4096}

let newline_chop bs =
  match Bytes.index_opt bs '\r' with
  | None -> None
  | Some i ->
    if Bytes.get bs (i + 1) = '\n' then
      Some (i + 1)
    else
      None

let get_buf_newlines reader =
  let s = Buffer.to_bytes reader.buf in
  match newline_chop s with
  | None -> None
  | Some i ->
    let line = Bytes.sub_string s 0 (i - 1) in
    Buffer.clear reader.buf;
    Buffer.add_bytes reader.buf (Bytes.sub s (i + 1) (Bytes.length s - i - 1));
    Some line

let line r =
  let rec loop () =
    match get_buf_newlines r with
    | Some line -> line
    | None -> (
      match Bytes.Reader.read r.reader with
      | s when Bytes.Slice.is_eod s ->
        if Buffer.length r.buf = 0 then
          raise End_of_file
        else
          let s = Buffer.contents r.buf in
          Buffer.clear r.buf;
          s
      | s -> (
        Bytes.Slice.add_to_buffer r.buf s;
        match get_buf_newlines r with
        | None -> loop ()
        | Some line -> line))
  in
  loop ()

let unsafe_take_and_refill r len =
  let s = Buffer.contents r.buf in
  let return = String.sub s 0 len in
  Buffer.clear r.buf;
  if String.length s > len then begin
    let add_back = String.sub s len (String.length s - len - 1) in
    Buffer.add_string r.buf add_back
  end;
  return

let take len r =
  let rec loop () =
    (* If we have enough data, return the length and restore the extra *)
    if Buffer.length r.buf >= len then begin
      unsafe_take_and_refill r len
    end
    else
      (* Otherwise read more data into the buffer *)
        match Bytes.Reader.read r.reader with
      | s when Bytes.Slice.is_eod s ->
        if Buffer.length r.buf < len then
          failwith "Not enough data!"
        else
          unsafe_take_and_refill r len
      | s ->
        Bytes.Slice.add_to_buffer r.buf s;
        loop ()
  in
  loop ()

module Header = struct
  type t = {
    content_length : int;
    content_type : string;
  }

  let content_type t = t.content_type
  let content_length t = t.content_length

  module Private = struct
    module Key = struct
      let content_length = "Content-Length"
      let content_type = "Content-Type"
    end
  end

  open Private

  let crlf = "\r\n"

  let to_string {content_length; content_type} =
    let b = Buffer.create 64 in
    let add = Buffer.add_string b in
    let line k v =
      add k;
      add ": ";
      add v;
      add crlf
    in
    line Key.content_length (string_of_int content_length);
    line Key.content_type content_type;
    add crlf;
    Buffer.contents b

  let default_content_type = "application/vscode-jsonrpc; charset=utf-8"

  let create ?(content_type = default_content_type) ~content_length () =
    {content_length; content_type}
end

exception Error of string

let () =
  Printexc.register_printer (function
    | Error msg -> Some ("Error: " ^ msg)
    | _ -> None)

let caseless_equal a b =
  if a == b then
    true
  else
    let len = String.length a in
    len = String.length b
    &&
    let stop = ref false in
    let idx = ref 0 in
    while (not !stop) && !idx < len do
      let c1 = String.unsafe_get a !idx in
      let c2 = String.unsafe_get b !idx in
      if Char.lowercase_ascii c1 <> Char.lowercase_ascii c2 then stop := true;
      incr idx
    done;
    not !stop

let content_type_lowercase =
  String.lowercase_ascii Header.Private.Key.content_type

let content_length_lowercase =
  String.lowercase_ascii Header.Private.Key.content_length

let read_header =
  let init_content_length = -1 in
  let rec loop chan content_length content_type =
    let line = line chan in
    match line with
    | exception End_of_file -> None
    | "" | "\r" -> Some (content_length, content_type)
    | line -> (
      match Astring.String.cut ~sep:":" line with
      | None -> loop chan content_length content_type
      | Some (k, v) ->
        let k = String.trim k in
        if
          caseless_equal k content_length_lowercase
          && content_length = init_content_length
        then
          let content_length = int_of_string_opt (String.trim v) in
          match content_length with
          | None -> raise (Error "Content-Length is invalid")
          | Some content_length -> loop chan content_length content_type
        else if caseless_equal k content_type_lowercase && content_type = None
        then
          let content_type = String.trim v in
          loop chan content_length (Some content_type)
        else
          loop chan content_length content_type)
  in
  fun chan ->
    let res = loop chan init_content_length None in
    match res with
    | None -> None
    | Some (content_length, content_type) ->
      let () =
        if content_length = init_content_length then
          raise (Error "content length absent")
      in
      Some (Header.create ?content_type ~content_length ())

let read chan =
  let header = read_header chan in
  match header with
  | None -> None
  | Some header -> (
    let len = Header.content_length header in
    let buf = take len chan in
    match buf with
    | exception End_of_file -> raise (Error "unable to read json")
    | buf ->
      let json = Yojson.Safe.from_string buf in
      Some (Jsonrpc.Packet.t_of_yojson json))

let write chan packet =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let data = Yojson.Safe.to_string json in
  let content_length = String.length data in
  let header = Header.create ~content_length () in
  List.iter (Bytes.Writer.write_string chan) [Header.to_string header; data]
