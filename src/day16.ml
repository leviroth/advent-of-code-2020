open! Core
open! Import

let test_case =
  {|class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12|}
;;

module Rule = struct
  type t = (int Maybe_bound.t * int Maybe_bound.t) list [@@deriving sexp]

  let parser =
    let open Angstrom in
    let one =
      map2
        (parse_int <* char '-')
        parse_int
        ~f:(fun start stop -> Maybe_bound.Incl start, Maybe_bound.Incl stop)
    in
    take_till (function
        | ':' -> true
        | _ -> false)
    *> string ": "
    *> sep_by1 (string " or ") one
  ;;
end

module Input = struct
  module T = struct
    type t =
      { rules : Rule.t list
      ; your_ticket : int list
      ; nearby_tickets : int list list
      }
    [@@deriving sexp]

    let parser =
      let open Angstrom in
      let ticket = sep_by1 (char ',') parse_int in
      let whitespace = take_while Char.is_whitespace in
      map3
        (many (Rule.parser <* whitespace))
        (string "your ticket:" *> whitespace *> ticket <* whitespace)
        (string "nearby tickets:" *> whitespace *> many (ticket <* whitespace))
        ~f:(fun rules your_ticket nearby_tickets ->
          { rules; your_ticket; nearby_tickets })
    ;;
  end

  include T
  include Input.Make_parseable (T)
end

let%expect_test _ =
  let input = Input.of_string test_case in
  print_s [%sexp (input : Input.t)];
  [%expect
    {|
    ((rules
      ((((Incl 1) (Incl 3)) ((Incl 5) (Incl 7)))
       (((Incl 6) (Incl 11)) ((Incl 33) (Incl 44)))
       (((Incl 13) (Incl 40)) ((Incl 45) (Incl 50)))))
     (your_ticket (7 1 14))
     (nearby_tickets ((7 3 47) (40 4 50) (55 2 20) (38 6 12)))) |}]
;;

module Part_01 = struct
  module Input = Input
  module Output = Int

  let solve ({ rules; nearby_tickets; your_ticket = _ } : Input.t) =
    let all_ticket_fields = List.concat nearby_tickets in
    let all_rule_clauses = List.concat rules in
    List.filter all_ticket_fields ~f:(fun ticket ->
        not
          (List.exists all_rule_clauses ~f:(fun (lower, upper) ->
               Maybe_bound.interval_contains_exn ~lower ~upper ~compare ticket)))
    |> List.sum (module Int) ~f:ident
  ;;
end

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 71 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01) ]
