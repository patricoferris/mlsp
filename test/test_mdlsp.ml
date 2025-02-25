open Bytesrw
module Io = Mlsp.Private.Io

let test_line () =
  let test_string = "hello world\r\n" in
  let reader = Io.make_reader (Bytes.Reader.of_string test_string) in
  let line = Io.line reader in
  Alcotest.(check string) "same string" "hello world" line

let test_lines () =
  let test_string = "hello world\r\nand another one\r\n" in
  let reader = Io.make_reader (Bytes.Reader.of_string test_string) in
  let line1 = Io.line reader in
  Alcotest.(check string) "same string 1" "hello world" line1;
  let line2 = Io.line reader in
  Alcotest.(check string) "same string 2" "and another one" line2

let test_take () =
  let test_string = "hello world\r\nand then some other stuff?" in
  let reader = Io.make_reader (Bytes.Reader.of_string test_string) in
  let line1 = Io.line reader in
  Alcotest.(check string) "same string 1" "hello world" line1;
  let and_ = Io.take 3 reader in
  Alcotest.(check string) "same string 2" "and" and_;
  let s_an = Io.take 3 reader in
  Alcotest.(check string) "same string 3" " th" s_an

let () =
  Alcotest.run "mlsp"
    [
      ( "io",
        [
          Alcotest.test_case "line" `Quick test_line;
          Alcotest.test_case "lines" `Quick test_lines;
          Alcotest.test_case "take" `Quick test_take;
        ] );
    ]
