module F = Format
open Lispy

let%expect_test "pp_empty_list" =
  let open Tokens in
  let open Types in
  let t = Expr [ Atom OP; Atom CP ] in
  F.printf "%a" Types.pp t;
  [%expect {|( )|}]
;;

let%expect_test "pp_one_atom" =
  let open Tokens in
  let open Types in
  let t = tokenize "(0.1)" |> parse in
  F.printf "%a" Types.pp t;
  [%expect {|( 0.1 )|}]
;;

let%expect_test "pp_define_expr" =
  let open Tokens in
  let open Types in
  let t = tokenize "(define r 0.1)" |> parse in
  F.printf "%a" Types.pp t;
  [%expect {|( define r 0.1 )|}]
;;

let%expect_test "pp_area_of_circle" =
  let open Tokens in
  let open Types in
  let t =
    Expr
      [ Atom OP
      ; Atom (S "begin")
      ; Expr [ Atom OP; Atom (S "define"); Atom (S "r"); Atom (N (I 10)); Atom CP ]
      ; Expr
          [ Atom OP
          ; Atom (S "*")
          ; Atom (S "pi")
          ; Expr [ Atom OP; Atom (S "*"); Atom (S "r"); Atom (S "r"); Atom CP ]
          ; Atom CP
          ]
      ; Atom CP
      ]
  in
  F.printf "%a" pp t;
  [%expect {|( begin ( define r 10 ) ( * pi ( * r r ) ) )|}]
;;

let%expect_test "tokenize" =
  let open Tokens in
  let t = tokenize "(begin (define r 10) (* pi (* r r)))" in
  F.printf "%a" pp t;
  [%expect
    {|"(", "begin", "(", "define", "r", "10", ")", "(", "*", "pi", "(", "*", "r", "r", ")", ")", ")"|}]
;;

let%expect_test "parse_unexpected_eof_simple" =
  let open Tokens in
  let open Types in
  try
    let t = tokenize "" |> parse in
    F.printf "%a" Types.pp t
  with
  | Failure msg ->
    F.printf "%s" msg;
    [%expect {|Unexpected EOF|}]
;;

let%expect_test "parse_unexpected_eof" =
  let open Tokens in
  let open Types in
  try
    let t = tokenize "(" |> parse in
    F.printf "%a" Types.pp t
  with
  | Failure msg ->
    F.printf "%s" msg;
    [%expect {|Unexpected EOF|}]
;;

let%expect_test "parse_single_closing_paren" =
  let open Tokens in
  let open Types in
  try
    let t = tokenize ")" |> parse in
    F.printf "%a" pp t
  with
  | Failure msg ->
    F.printf "%s" msg;
    [%expect {|Syntax error: Expected "(", found ")"|}]
;;

let%expect_test "parse_area_of_circle" =
  let open Tokens in
  let open Types in
  let t = tokenize "(begin (define r 10) (* pi (* r r)))" |> parse in
  F.printf "%a" pp t;
  [%expect {|(begin (define r 10) (* pi (* r r)))|}]
;;
