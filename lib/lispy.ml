module F = Format

module Tokens = struct
  type t = string list

  let pp fmt t =
    F.pp_print_list
      ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
      (fun fmt t -> F.fprintf fmt "\"%s\"" t)
      fmt
      t
  ;;

  (* Tokenize program from raw string *)
  let tokenize s : t =
    Str.global_replace (Str.regexp_string "(") " ( " s
    |> Str.global_replace (Str.regexp_string ")") " ) "
    |> String.split_on_char ' '
    |> List.filter (fun token -> token <> String.empty)
  ;;
end

module Types = struct
  type t =
    | A of atom
    | L of t list

  and atom =
    | S of string
    | N of number

  and number =
    | F of float
    | I of int

  let rec pp fmt t =
    match t with
    | A a ->
      (match a with
       | S s -> F.pp_print_string fmt s
       | N n ->
         (match n with
          | F f -> F.pp_print_float fmt f
          | I i -> F.pp_print_int fmt i))
    | L l -> F.fprintf fmt "(%a)" pp_list l

  and pp_list fmt l =
    F.pp_print_list
      ~pp_sep:(fun fmt () -> F.fprintf fmt " ")
      (fun fmt t -> pp fmt t)
      fmt
      l
  ;;

  let rec parse dbg tokens =
    match tokens with
    | [] -> raise (Failure "Unexpected EOF")
    | "(" :: tl ->
      if dbg then F.printf "Open : %a\n" Tokens.pp tl;
      (match tl with
       | [] -> raise (Failure "Syntax error")
       | tl -> parse dbg tl)
    | ")" :: tl ->
      if dbg then F.printf "Close: %a\n" Tokens.pp tl;
      (match tl with
       | [] -> raise (Failure "Syntax error")
       | tl -> parse dbg tl)
    | _ ->
      let expr, tl = parse_list dbg tokens in
      if dbg
      then (
        F.printf "Expr : %a\n" pp (L expr);
        F.printf "Rem  : %a\n" Tokens.pp tl);
      (match tl with
       | [] -> L expr
       | tl -> L (expr @ [ parse dbg tl ]))

  and parse_list dbg tokens =
    match tokens with
    | [] -> [], []
    | "(" :: _ -> [ parse dbg tokens ], []
    | ")" :: tl -> [], tl
    | token :: tl ->
      if dbg then F.printf "List : %a\n" Tokens.pp tokens;
      let expr, tl = parse_list dbg tl in
      parse_atom token :: expr, tl

  and parse_atom token =
    try
      let i = int_of_string token in
      A (N (I i))
    with
    | Failure _ ->
      (try
         let f = float_of_string token in
         A (N (F f))
       with
       | Failure _ -> A (S token))

  and is_paren p = p = "(" || p = ")"
  and dbg_format p = if p = "(" then "Open : %a\n" else "Close: %a\n"
end
