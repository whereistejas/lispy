module F = Format
open Lispy

let%expect_test "tokenize" =
  let t = Tokens.tokenize "(begin (define r 10) (* pi (* r r)))" in
  F.printf "[%a]" Tokens.pp t;
  [%expect
    {|["(", "begin", "(", "define", "r", "10", ")", "(", "*", "pi", "(", "*", "r", "r", ")", ")", ")"]|}]
;;

let%expect_test "pretty_print" =
  let t =
    Types.Exp
      (Exp.L
         [ Symbol "begin"
         ; Types.Exp (Exp.L [ Symbol "define"; Symbol "r"; Number (Number.I 10) ])
         ])
  in
  F.printf "%a" Types.pp t;
  [%expect {|(begin (define r 10))|}]
;;
