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

  let rec pp fmt t =
    match t with
    | Atom a -> F.fprintf fmt "%s" (Tokens.to_string a)
    | Expr e -> F.fprintf fmt "%a" pp_l e

  and pp_l e =
    F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt " ") (fun fmt x -> pp fmt x) e
  ;;

  let parse tokens =
    let rec helper acc tokens =
      match tokens with
      | [] -> raise (Failure "Unexpected EOF")
      | t ->
        (match parse_expr acc t with
         | Ok (parsed, _rest) -> Expr parsed
         | Error exn -> raise exn)
    and parse_expr acc tokens =
      let ( let* ) x f =
        match x with
        | Ok x -> f x
        | Error _ as e -> e
      in
      let* parsed, rest = parse_token Tokens.OP [] tokens in
      let* parsed, rest = parse_atom parsed rest in
      let* parsed, rest = parse_atom parsed rest in
      let* parsed, rest = parse_atom parsed rest in
      let* parsed, rest = parse_token Tokens.CP parsed rest in
      Ok (Expr (List.rev parsed) :: acc, rest)
    and parse_token token acc tokens =
      match tokens with
      | [] -> Error (Failure "Unexpected EOF")
      | t :: tl when t = token -> Ok (Atom t :: acc, tl)
      | t :: _ -> Error (unexpected_token token t)
    and parse_atom acc tokens =
      match tokens with
      | [] -> Error (Failure "Unexpected EOF")
      | Tokens.OP :: _ -> (parse_expr [@tailcall]) acc tokens
      | (Tokens.S _ as token) :: rest -> Ok (Atom token :: acc, rest)
      | (Tokens.N _ as token) :: rest -> Ok (Atom token :: acc, rest)
      | t :: _ -> Error (unexpected_token (Tokens.S "Symbol|Number") t)
    in
    helper [] tokens
  ;;
end
