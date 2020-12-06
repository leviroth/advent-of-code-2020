open! Core
open! Import

module Group = struct
  type t = Char.Set.t list [@@deriving sexp]

  let parser =
    let open Angstrom in
    many1
      (map
         (take_while1 Char.is_alpha <* (end_of_input <|> end_of_line))
         ~f:(fun answers -> Char.Set.of_list (String.to_list answers)))
  ;;

  let%expect_test _ =
    let test_case = {|abcx
abcy
abcz
|} in
    Angstrom.parse_string ~consume:All parser test_case
    |> Result.ok_or_failwith
    |> sexp_of_t
    |> print_s;
    [%expect {| ((a b c x) (a b c y) (a b c z)) |}]
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Group)
  module Output = Int

  let test_case = {|abc

a
b
c

ab
ac

a
a
a
a

b
|}
end

let%expect_test _ =
  let open Common in
  Input.of_string test_case |> [%sexp_of: Group.t list] |> print_s;
  [%expect {| (((a b c)) ((a) (b) (c)) ((a b) (a c)) ((a) (a) (a) (a)) ((b))) |}]
;;

module Part_01 = struct
  include Common

  let solve = List.sum (module Int) ~f:(fun one -> Set.length (Char.Set.union_list one))

  let%expect_test _ =
    let open Common in
    let input = Input.of_string test_case in
    print_s [%sexp (solve input : int)];
    [%expect {| 11 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
