module F = Format

module Tokens = struct
  type t =
    | OP
    | S of string
    | N of number
    | CP

  and number =
    | F of float
    | I of int

  let to_string t =
    match t with
    | OP -> "("
    | CP -> ")"
    | S s -> s
    | N n ->
      (match n with
       | F f -> Float.to_string f
       | I i -> Int.to_string i)
  ;;

  let pp fmt tokens =
    F.pp_print_list
      ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
      (fun fmt token -> F.fprintf fmt "\"%s\"" (to_string token))
      fmt
      tokens
  ;;

  (* Tokenize program from raw string *)
  let tokenize s : t list =
    Str.global_replace (Str.regexp_string "(") " ( " s
    |> Str.global_replace (Str.regexp_string ")") " ) "
    |> String.split_on_char ' '
    |> List.filter (fun token -> token <> String.empty)
    |> List.map (fun token ->
      match token with
      | "(" -> OP
      | ")" -> CP
      | _ ->
        (try
           let i = int_of_string token in
           N (I i)
         with
         | Failure _ ->
           (try
              let f = float_of_string token in
              N (F f)
            with
            | Failure _ -> S token)))
  ;;
end

module Types = struct
  type t =
    | Atom of Tokens.t
    | Expr of t list

  let unexpected_token expected actual =
    Failure
      ("Syntax error: Expected \""
       ^ Tokens.to_string expected
       ^ "\", found \""
       ^ Tokens.to_string actual
       ^ "\"")
  ;;

  let unexpected_eof = Failure "Unexpected EOF"

  let rec pp fmt t =
    match t with
    | Atom a -> F.fprintf fmt "%s" (Tokens.to_string a)
    | Expr e -> F.fprintf fmt "%a" pp_l e

  and pp_l e =
    F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt " ") (fun fmt x -> pp fmt x) e
  ;;

  let rec parse_token token acc tokens =
    match tokens with
    | [] -> raise unexpected_eof
    | t :: tl when t = token -> Ok (Atom t :: acc, tl)
    | t :: _ -> Error (unexpected_token token t)

  and parse_atom acc tokens =
    match tokens with
    | [] -> Error unexpected_eof
    | (Tokens.S _ as token) :: tl -> Ok (Atom token :: acc, tl)
    | (Tokens.N _ as token) :: tl -> Ok (Atom token :: acc, tl)
    | t :: _ -> Error (unexpected_token (Tokens.S "Symbol|Number") t)

  and parse_expr acc tokens =
    (* Use [custom operator] for joining parsers so that we can remove this `List.rev`.
       [custom operator]: https://ocaml.org/docs/operators#operator-associativity-and-precedence
    *)
    match parse_token Tokens.OP [] tokens with
    | Ok (acci, tl) ->
      (match parse_atom acci tl with
       | Ok (acci, tl) ->
         (match parse_atom acci tl with
          | Ok (acci, tl) ->
            (match parse_atom acci tl with
             | Ok (acci, tl) ->
               (match parse_token Tokens.CP acci tl with
                | Ok (acci, tl) -> Ok (Expr (List.rev acci) :: acc, tl)
                | Error exn -> Error exn)
             | Error exn -> Error exn)
          | Error exn -> Error exn)
       | Error exn -> Error exn)
    | Error exn -> Error exn

  and parse_literal tokens =
    match parse_token Tokens.OP [] tokens with
    | Ok (acc, tl) ->
      (match parse_atom acc tl with
       | Ok (acc, tl) ->
         (match parse_token Tokens.CP acc tl with
          | Ok (acc, _) -> Ok (Expr (List.rev acc))
          | Error exn -> Error exn)
       | Error exn -> Error exn)
    | Error exn -> Error exn

  and parse tokens =
    match parse_expr [] tokens with
    | Ok (acc, tl) ->
      assert (tl = []);
      Expr acc
    | Error _ ->
      (match parse_literal tokens with
       | Ok literal -> literal
       | Error exn -> raise exn)
  ;;
end
