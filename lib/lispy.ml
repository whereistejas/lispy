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

  let pp t = F.pp_print_string F.std_formatter t
  let create token : t = token
end

module Number = struct
  type t =
    | I of int
    | F of float

  let pp = function
    | F f -> F.pp_print_float F.std_formatter f
    | I i -> F.pp_print_int F.std_formatter i
  ;;
end

module Atom = struct
  type t =
    | S of Symbol.t
    | N of Number.t

  let pp = function
    | S s -> Symbol.pp s
    | N n -> Number.pp n
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
    | Exp of t Exp.t
    | Env of (string * string) list

  let rec pp fmt = function
    | Symbol symbol -> Symbol.pp symbol
    | Number number -> Number.pp number
    | Atom atom -> Atom.pp atom
    | List l -> pp_list fmt l
    | Exp exp ->
      (match exp with
       | Exp.A a -> Atom.pp a
       | Exp.L l -> F.fprintf F.std_formatter "(%a)" pp_inner (List l))
    | Env env ->
      List.iter (fun (key, value) -> F.fprintf F.std_formatter "%s %s" key value) env

  and pp_list fmt l =
    F.pp_print_list
      ~pp_sep:(fun fmt () -> F.fprintf fmt " ")
      (fun fmt t -> F.fprintf fmt "%a" pp t)
      fmt
      l

  and pp_inner fmt t =
    match t with
    | Symbol _ | Number _ | Atom _ | Env _ -> pp fmt t
    | List l -> F.fprintf fmt "%a" pp_list l
    | Exp _ -> F.fprintf fmt "%a" pp t
  ;;

  let rec parse tokens =
    let () = F.printf "token: [%a]\n" Tokens.pp tokens in
    match tokens with
    | [] -> raise (Failure "Unexpected EOF")
    | "(" :: tail ->
      let acc =
        let exception Return of t list in
        try
          List.fold_left
            (fun acc x ->
              match x with
              | ")" -> raise (Return acc)
              | _ ->
                (match tail with
                 | _ :: tl -> parse tl :: acc
                 | [] -> []))
            []
            tail
        with
        | Return ret -> ret
      in
      Exp (Exp.L acc)
    | token :: _ -> Atom (Atom.parse token)
  ;;
end
