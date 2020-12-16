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
  type t =
    { name : string
    ; ranges : (int Maybe_bound.t * int Maybe_bound.t) list
    }
  [@@deriving sexp, fields]

  let parser =
    let open Angstrom in
    let one =
      map2
        (parse_int <* char '-')
        parse_int
        ~f:(fun start stop -> Maybe_bound.Incl start, Maybe_bound.Incl stop)
    in
    map2
      (take_till (function
           | ':' -> true
           | _ -> false)
      <* string ": ")
      (sep_by1 (string " or ") one)
      ~f:(fun name ranges -> { name; ranges })
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
      (((name class) (ranges (((Incl 1) (Incl 3)) ((Incl 5) (Incl 7)))))
       ((name row) (ranges (((Incl 6) (Incl 11)) ((Incl 33) (Incl 44)))))
       ((name seat) (ranges (((Incl 13) (Incl 40)) ((Incl 45) (Incl 50)))))))
     (your_ticket (7 1 14))
     (nearby_tickets ((7 3 47) (40 4 50) (55 2 20) (38 6 12)))) |}]
;;

module Common = struct
  module Input = Input
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve ({ rules; nearby_tickets; your_ticket = _ } : Input.t) =
    let all_ticket_fields = List.concat nearby_tickets in
    let all_rule_clauses = List.concat_map rules ~f:Rule.ranges in
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

module Part_02 = struct
  include Common

  let matches_rule field ({ ranges; _ } : Rule.t) =
    List.exists ranges ~f:(fun (lower, upper) ->
        Maybe_bound.interval_contains_exn ~lower ~upper ~compare field)
  ;;

  let solve ({ rules; nearby_tickets; your_ticket } : Input.t) =
    let valid_tickets =
      List.filter nearby_tickets ~f:(fun ticket ->
          List.for_all ticket ~f:(fun field -> List.exists rules ~f:(matches_rule field)))
    in
    let possible_positions_by_rule =
      let length = List.length rules in
      let range = List.range 0 length in
      Int.Table.of_alist_exn (List.map range ~f:(fun i -> i, Int.Hash_set.of_list range))
    in
    List.iter valid_tickets ~f:(fun ticket ->
        List.iteri ticket ~f:(fun fieldi field ->
            List.iteri rules ~f:(fun rulei rule ->
                match matches_rule field rule with
                | true -> ()
                | false ->
                  let set = Hashtbl.find_exn possible_positions_by_rule rulei in
                  Hash_set.remove set fieldi)));
    let actual_positions_by_rule = Int.Table.create () in
    let rec loop () =
      let fully_constrained =
        Hashtbl.filter_map possible_positions_by_rule ~f:(fun set ->
            match Hash_set.to_list set with
            | [ x ] -> Some x
            | _ :: _ -> None
            | [] -> assert false)
      in
      match Hashtbl.length fully_constrained with
      | 0 -> ()
      | _ ->
        Hashtbl.iteri fully_constrained ~f:(fun ~key:rule ~data:position ->
            Hashtbl.add_exn actual_positions_by_rule ~key:rule ~data:position;
            Hashtbl.remove possible_positions_by_rule rule;
            Hashtbl.iter possible_positions_by_rule ~f:(fun set ->
                Hash_set.remove set position));
        loop ()
    in
    loop ();
    let departure_rules =
      List.filter_mapi rules ~f:(fun i ({ name; _ } : Rule.t) ->
          match String.is_prefix name ~prefix:"departure" with
          | true -> Some i
          | false -> None)
    in
    let positions =
      List.map departure_rules ~f:(Hashtbl.find_exn actual_positions_by_rule)
    in
    List.map positions ~f:(List.nth_exn your_ticket) |> List.reduce_exn ~f:( * )
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
