module F = Format

module Tokens = struct
  type t = string list

  (* Pretty print tokens*)
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

  and number =
    | F of float
    | I of int

  and atom =
    | S of string
    | N of number

  let rec pp_number fmt n =
    match n with
    | F f -> F.pp_print_float fmt f
    | I i -> F.pp_print_int fmt i

  and pp_atom fmt a =
    match a with
    | S s -> F.pp_print_string fmt s
    | N n -> pp_number fmt n

  and pp_list fmt l =
    F.pp_print_list
      ~pp_sep:(fun fmt () -> F.fprintf fmt " ")
      (fun fmt t -> pp fmt t)
      fmt
      l

  and pp fmt t =
    match t with
    | A a -> pp_atom fmt a
    | L l -> F.fprintf fmt "(%a)" pp_list l
  ;;

  let rec parse dbg tokens =
    match tokens with
    | [] -> raise (Failure "Unexpected EOF")
    | "(" :: tl ->
      let expr, remaining = parse_list tl in
      (match remaining with
       | [] | ")" :: [] -> L expr
       | ")" :: tl ->
         if dbg then F.eprintf "E: %a\n" pp (L expr);
         L (parse dbg tl :: expr)
       | _ -> raise (Failure ". Syntax error"))
    | t ->
      if dbg then F.printf "T: %a\n" Tokens.pp t;
      raise (Failure "Syntax error")

  (* returns a list of atoms and remaining tokens *)
  and parse_list tokens =
    match tokens with
    | [] -> [], []
    | ")" :: tokens -> [], tokens
    | token :: tl ->
      let expr, remaining = parse_list tl in
      parse_atom token :: expr, remaining

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
