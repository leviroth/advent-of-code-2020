open! Core
open! Import

module Solve_gen (Expression : Input.Parseable.Basic) = struct
  module Input = Input.Make_parseable_many (Expression)
  module Output = Int

  let solve = List.sum (module Int) ~f:ident
end

module Expression = struct
  type t = int

  open Angstrom

  let ws = skip_while Char.is_whitespace
  let parens p = char '(' *> p <* ws <* char ')'
  let add = ws *> char '+' *> ws *> return ( + )
  let mul = ws *> char '*' *> ws *> return ( * )

  let chainl1 e op =
    let open Angstrom.Let_syntax in
    let rec go acc =
      (let%bind f = op in
       let%bind x = e in
       go (f acc x))
      <|> return acc
    in
    let%bind.Angstrom init = e in
    go init
  ;;
end

module Part_01 = Solve_gen (struct
  include Expression

  let parser =
    let open Angstrom in
    fix (fun expression ->
        let term = parens expression <|> parse_int in
        chainl1 term (add <|> mul))
  ;;
end)

let test_cases =
  [ "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  ; "2 * 3 + (4 * 5)"
  ; "5 + (8 * 3 + 9 + 3 * 4 * 3)"
  ; "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
  ; "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  ]
;;

let%expect_test _ =
  List.iter test_cases ~f:(fun case ->
      let input = Part_01.Input.of_string case in
      printf "%d\n" (Part_01.solve input));
  [%expect {|
    13632
    26
    437
    12240
    13632 |}]
;;

module Part_02 = Solve_gen (struct
  include Expression

  let parser =
    let open Angstrom in
    fix (fun expression ->
        let term = parens expression <|> parse_int in
        let term = chainl1 term add in
        chainl1 term mul)
  ;;
end)

let%expect_test _ =
  List.iter test_cases ~f:(fun case ->
      let input = Part_02.Input.of_string case in
      printf "%d\n" (Part_02.solve input));
  [%expect {|
    23340
    46
    1445
    669060
    23340 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
