module F = Format
open Lispy

let%expect_test "tokenize" =
  let open Tokens in
  let t = tokenize "(begin (define r 10) (* pi (* r r)))" in
  F.printf "%a" pp t;
  [%expect
    {|"(", "begin", "(", "define", "r", "10", ")", "(", "*", "pi", "(", "*", "r", "r", ")", ")", ")"|}]
;;

let%expect_test "pp_empty_list" =
  let open Types in
  let t = L [] in
  F.printf "%a" pp t;
  [%expect {|()|}]
;;

let%expect_test "parse_empty_string" =
  let open Tokens in
  let open Types in
  try
    let t = tokenize "" |> parse false in
    F.printf "%a" pp t
  with
  | Failure msg ->
    F.printf "%s" msg;
    [%expect {|Unexpected EOF|}]
;;

let%expect_test "pp_one_atom" =
  let open Types in
  let t = L [ A (N (F 0.1)) ] in
  F.printf "%a" pp t;
  [%expect {|(0.1)|}]
;;

let%expect_test "parse_single_closing_paren" =
  let open Tokens in
  let open Types in
  try
    let t = tokenize ")" |> parse false in
    F.printf "%a" pp t
  with
  | Failure msg ->
    F.printf "%s" msg;
    [%expect {|Syntax error|}]
;;

let%expect_test "parse_single_opening_paren" =
  let open Tokens in
  let open Types in
  try
    let t = tokenize "(" |> parse false in
    F.printf "%a" pp t
  with
  | Failure msg ->
    F.printf "%s" msg;
    [%expect {|Syntax error|}]
;;

let%expect_test "pp_define_radius" =
  let open Types in
  let t = L [ A (S "define"); A (S "r"); A (N (I 10)) ] in
  F.printf "%a" pp t;
  [%expect {|(define r 10)|}]
;;

let%expect_test "parse_define_radius" =
  let open Tokens in
  let open Types in
  let t = tokenize "(define r 10)" |> parse false in
  F.printf "%a" pp t;
  [%expect {|(define r 10)|}]
;;

let%expect_test "pp_area_of_circle" =
  let open Types in
  let t =
    L
      [ A (S "begin")
      ; L [ A (S "define"); A (S "r"); A (N (I 10)) ]
      ; L [ A (S "*"); A (S "pi"); L [ A (S "*"); A (S "r"); A (S "r") ] ]
      ]
  in
  F.printf "%a" pp t;
  [%expect {|(begin (define r 10) (* pi (* r r)))|}]
;;

let%expect_test "parse_area_of_circle" =
  let open Tokens in
  let open Types in
  let t = tokenize "(begin (define r 10) (* pi (* r r)))" |> parse true in
  F.printf "%a" pp t;
  [%expect {|(begin (define r 10) (* pi (* r r)))|}]
;;
