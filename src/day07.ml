open! Core
open! Import

module Rule = struct
  type t = string * (int * string) list [@@deriving sexp]

  open Angstrom

  let word = take_while Char.is_alpha <?> "word"
  let space = char ' ' <?> "space"
  let bag = string "bags" <|> string "bag" <?> "bag"

  let color =
    map2
      (word <* space)
      (word <* space <* bag)
      ~f:(fun a b -> String.concat [ a; b ] ~sep:" ")
    <?> "color"
  ;;

  let edge = both (parse_int <* space) color <?> "edge"
  let nothing = string "no other bags" *> return []
  let edges = nothing <|> sep_by1 (string ", ") edge <?> "edges"
  let parser = both (color <* string " contain ") (edges <* char '.')
end

module Common = struct
  module Input = Input.Make_parseable_many (Rule)
  module Output = Int
end

let test_case =
  {|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
|}
;;

let%expect_test _ =
  let input = Common.Input.of_string test_case in
  print_s [%sexp (input : Rule.t list)];
  [%expect
    {|
    (("light red" ((1 "bright white") (2 "muted yellow")))
     ("dark orange" ((3 "bright white") (4 "muted yellow")))
     ("bright white" ((1 "shiny gold")))
     ("muted yellow" ((2 "shiny gold") (9 "faded blue")))
     ("shiny gold" ((1 "dark olive") (2 "vibrant plum")))
     ("dark olive" ((3 "faded blue") (4 "dotted black")))
     ("vibrant plum" ((5 "faded blue") (6 "dotted black"))) ("faded blue" ())
     ("dotted black" ())) |}]
;;

module Part_01 = struct
  include Common

  let build_graph input =
    List.concat_map input ~f:(fun (outer, inners) ->
        List.map inners ~f:(fun (_count, inner) -> inner, outer))
    |> String.Map.of_alist_multi
  ;;

  let rec fold graph ~node ~init ~f =
    let init = f init node in
    match Map.find graph node with
    | None -> init
    | Some children ->
      List.fold children ~init ~f:(fun init node -> fold graph ~node ~init ~f)
  ;;

  let solve input =
    let reachable =
      build_graph input
      |> fold ~node:"shiny gold" ~init:String.Set.empty ~f:String.Set.add
    in
    Set.length reachable - 1
  ;;
end

let%expect_test _ =
  let input = Common.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 4 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01) ]
