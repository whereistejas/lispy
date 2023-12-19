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

module Symbol = struct
  type t = string

  let pp fmt t = F.pp_print_string fmt t
  let create token : t = token
end

module Number = struct
  type t =
    | I of int
    | F of float

  let pp fmt = function
    | F f -> F.pp_print_float fmt f
    | I i -> F.pp_print_int fmt i
  ;;
end

module Atom = struct
  type t =
    | S of Symbol.t
    | N of Number.t

  let pp fmt = function
    | S s -> Symbol.pp fmt s
    | N n -> Number.pp fmt n
  ;;

  let parse token : t =
    try
      let i = int_of_string token in
      N (Number.I i)
    with
    | Failure _ ->
      (try
         let f = float_of_string token in
         N (Number.F f)
       with
       | Failure _ -> S (Symbol.create token))
  ;;
end

module Exp = struct
  type 't t =
    | A of Atom.t
    | L of 't list
end

let rec split_at n l =
  match l with
  | [] -> [], []
  | hd :: tl ->
    let before, after = split_at (n - 1) tl in
    if n > 0 then hd :: before, after else before, hd :: after
;;

module Types = struct
  type t =
    | Symbol of Symbol.t
    | Number of Number.t
    | Atom of Atom.t
    | List of t list
    | Exp of t Exp.t (* An expression can be an Atom or a List of t*)
    | Env of (string * string) list
    | End

  let rec pp fmt = function
    | Symbol s -> Symbol.pp fmt s
    | Number n -> Number.pp fmt n
    | Atom a -> Atom.pp fmt a
    | List l -> pp_list fmt l
    | Exp exp ->
      (match exp with
       | Exp.A a -> F.fprintf fmt "(%a)" Atom.pp a
       | Exp.L l -> F.fprintf fmt "(%a)" pp_list l)
    | Env env -> List.iter (fun (key, value) -> F.fprintf fmt "%s %s" key value) env
    | End -> ()

  and pp_list fmt l =
    List.filter (fun x -> x <> End) l
    |> F.pp_print_list
         ~pp_sep:(fun fmt () -> F.fprintf fmt " ")
         (fun fmt t -> F.fprintf fmt "%a" pp t)
         fmt

  and pp_inner fmt t =
    match t with
    | Symbol _ | Number _ | Atom _ | Env _ | End -> pp fmt t
    | List l -> F.fprintf fmt "%a" pp_list l
    | Exp _ -> F.fprintf fmt "%a" pp t
  ;;

  let rec parse tokens =
    match tokens with
    | [] -> End
    | "(" :: tl ->
      let e, remaining = parse_expr tl in
      let l = e @ [ parse remaining ] in
      Exp (Exp.L l)
    | ")" :: tl -> parse tl
    | _ -> raise (Failure "unreachable")

  (* returns the parsed expression and remaining tokens *)
  and parse_expr tokens =
    match tokens with
    | [] -> [], []
    | "(" :: tl ->
      let e, r = parse_expr tl in
      Exp (Exp.L e) :: [], r
    | ")" :: tl -> [], tl
    | token :: tl ->
      let p, r = parse_expr tl in
      Atom (Atom.parse token) :: p, r
  ;;
end
