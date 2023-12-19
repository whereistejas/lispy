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
         ; Types.Exp
             (Exp.L
                [ Symbol "*"
                ; Symbol "pi"
                ; Types.Exp (Exp.L [ Symbol "*"; Symbol "r"; Symbol "r" ])
                ])
         ])
  in
  F.printf "%a" Types.pp t;
  [%expect {|(begin (define r 10) (* pi (* r r)))|}]
;;

let%expect_test "parse" =
  let t = Tokens.tokenize "(begin (define r 10) (* pi (* r r)))" |> Types.parse in
  F.printf "%a" Types.pp t;
  [%expect {|(begin (define r 10) (* pi (* r r)))|}]
;;
