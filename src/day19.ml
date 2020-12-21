open! Core
open! Import

module Tree = struct
  type t =
    | Char of char
    | Reference of int
    | Sequence of t list
    | Choice of t list
  [@@deriving variants, sexp]
end

module Rules = struct
  type t = string Angstrom.t

  open Angstrom

  let ws = skip_many (char ' ')
  let parse_single_char = map (char '"' *> any_char <* char '"') ~f:Tree.char
  let parse_rule_reference = map (parse_int <* ws) ~f:Tree.reference
  let sequence = map (many1 parse_rule_reference) ~f:Tree.sequence
  let choice = map (sep_by1 (char '|' <* ws) sequence) ~f:Tree.choice

  let one_line =
    both (parse_int <* char ':' <* ws) (parse_single_char <|> choice) <* char '\n'
  ;;

  let parser' = many1 one_line

  let parser =
    map parser' ~f:(fun lines ->
        let by_number = Int.Map.of_alist_exn lines in
        let rec of_rule (rule : Tree.t) =
          match rule with
          | Char c -> char c |> map ~f:Char.to_string
          | Reference n -> of_rule (Map.find_exn by_number n)
          | Sequence l -> List.map l ~f:of_rule |> list |> map ~f:String.concat
          | Choice l -> List.map l ~f:of_rule |> Angstrom.choice
        in
        of_rule (Map.find_exn by_number 0))
  ;;

  let parses t string = Angstrom.parse_string ~consume:All t string |> Result.is_ok
end

let test_case = {|0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b"
|}

let%expect_test _ =
  let by_number =
    Angstrom.parse_string ~consume:All Rules.parser' test_case
    |> Result.ok_or_failwith
    |> Int.Map.of_alist_exn
  in
  print_s [%message (by_number : Tree.t Int.Map.t)];
  [%expect
    {|
    (by_number
     ((0 (Choice ((Sequence ((Reference 1) (Reference 2)))))) (1 (Char a))
      (2
       (Choice
        ((Sequence ((Reference 1) (Reference 3)))
         (Sequence ((Reference 3) (Reference 1))))))
      (3 (Char b)))) |}]
;;

let test_case =
  {|0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
|}
;;

module Input = struct
  module T = struct
    type t =
      { rules : Rules.t
      ; strings : string list
      }

    let parser =
      let open Angstrom in
      map2
        (Rules.parser <* char '\n')
        (many (take_while1 (Fn.non (Char.equal '\n')) <* char '\n'))
        ~f:(fun rules strings -> { rules; strings })
    ;;
  end

  include T
  include Input.Make_parseable (T)
end

module Part_01 = struct
  module Input = Input
  module Output = Int

  let solve ({ rules; strings } : Input.t) = List.count strings ~f:(Rules.parses rules)
end

let%expect_test _ =
  let ({ strings; rules } : Input.t) =
    Angstrom.parse_string ~consume:All Input.parser test_case |> Result.ok_or_failwith
  in
  print_s [%sexp (strings : string list)];
  [%expect {|
    (ababbb bababa abbbab aaabbb aaaabbb) |}];
  let matching =
    List.filter strings ~f:(fun string ->
        Angstrom.parse_string ~consume:All rules string |> Result.is_ok)
  in
  print_s [%sexp (matching : string list)];
  [%expect {| (ababbb abbbab) |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01) ]
