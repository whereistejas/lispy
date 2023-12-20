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

  let rec split_at n l =
    match l with
    | [] -> [], []
    | hd :: tl ->
      let before, after = split_at (n - 1) tl in
      if n > 0 then hd :: before, after else before, hd :: after
  ;;

  let rec parse dbg tokens =
    match tokens with
    | [] -> raise (Failure "Unexpected EOF")
    | "(" :: tl ->
      if dbg then F.printf "Open: %a\n" Tokens.pp tl;
      parse_expr dbg tl
    | t ->
      if dbg then F.printf "ParseE: %a\n" Tokens.pp t;
      parse_expr dbg tokens

  and parse_expr dbg tokens =
    match tokens with
    | "(" :: tl ->
      if dbg then F.printf "Open : %a\n" Tokens.pp tl;
      parse_expr dbg tl
    | ")" :: tl ->
      if dbg then F.printf "Close: %a\n" Tokens.pp tl;
      parse dbg tl
    | tokens ->
      if dbg then F.printf "ParseL: %a\n" Tokens.pp tokens;
      let expr, tl = parse_list dbg tokens in
      if dbg then F.printf "Expr : %a\n" pp (L expr);
      (match tl with
       | [] -> L expr
       | tl -> L (expr @ [ parse_expr dbg tl ]))

  and parse_list dbg tokens =
    match tokens with
    | [] -> [], []
    | ")" :: tl -> [], tl
    | "(" :: tl -> [ parse_expr dbg tl ], []
    | token :: tl ->
      if dbg then F.printf "List : %a\n" Tokens.pp tl;
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
  ;;
end

(* let rec parse dbg tokens =
   if dbg then F.printf "Start: %a\n" Tokens.pp tokens;
   match tokens with
   | [] -> raise (Failure "Unexpected EOF")
   | "(" :: _ -> parse_expr dbg tokens
   | t ->
   if dbg then F.printf "Error: %a\n" Tokens.pp t;
   raise (Failure "Syntax error") *)

(* and parse_expr dbg tokens =
    match tokens with
    | ")" :: tl ->
      if dbg then F.printf "Close: %a\n" Tokens.pp tl;
      L [ parse dbg tl ]
    | "(" :: tl ->
      if dbg then F.printf "Open : %a\n" Tokens.pp tl;
      parse_expr dbg tl
    | _ ->
      let expr, tl = parse_list dbg tokens in
      if dbg then F.printf "Expr : %a\n" pp (L expr);
      L (expr @ [ parse dbg tl ]) *)
(*
   (** Returns a list of atoms and remaining tokens *)
   and parse_list dbg tokens =
   match tokens with
   | [] -> [], []
   (* This branch will only be called through recursive calls by self *)
   | "(" :: _ -> [ parse_expr dbg tokens ], []
   | ")" :: _ -> [ parse_expr dbg tokens ], []
   | token :: tl ->
   assert (token <> "(");
   let expr, remaining = parse_list dbg tl in
   parse_atom token :: expr, remaining *)
