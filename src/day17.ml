open! Core
open! Import

module Coord = struct
  module T = struct
    type t = int * int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let add (a, b, c) (a', b', c') = a + a', b + b', c + c'
end

let test_case = {|.#.
..#
###|}

module Space = struct
  type t = Coord.Set.t [@@deriving sexp]

  let parser =
    let open Angstrom in
    let cell = char '#' *> return true <|> char '.' *> return false in
    let one_row = many1 cell <* take_while Char.is_whitespace in
    let all = many one_row in
    map all ~f:(fun all ->
        List.mapi all ~f:(fun row_num row ->
            List.mapi row ~f:(fun col_num cell -> (col_num, row_num, 0), cell))
        |> List.concat
        |> List.filter ~f:snd
        |> List.map ~f:fst
        |> Coord.Set.of_list)
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Space)
  module Output = Int
end

module Part_01 = struct
  include Common

  let offsets =
    let range = [ -1; 0; 1 ] in
    let all =
      let open List.Let_syntax in
      let%bind x = range
      and y = range
      and z = range in
      [ x, y, z ]
    in
    Set.remove (Coord.Set.of_list all) (0, 0, 0) |> Set.to_list
  ;;

  let%expect_test _ =
    print_s [%sexp (offsets : Coord.t list)];
    [%expect {|
      ((-1 -1 -1) (-1 -1 0) (-1 -1 1) (-1 0 -1) (-1 0 0) (-1 0 1) (-1 1 -1)
       (-1 1 0) (-1 1 1) (0 -1 -1) (0 -1 0) (0 -1 1) (0 0 -1) (0 0 1) (0 1 -1)
       (0 1 0) (0 1 1) (1 -1 -1) (1 -1 0) (1 -1 1) (1 0 -1) (1 0 0) (1 0 1)
       (1 1 -1) (1 1 0) (1 1 1)) |}]
  ;;

  let neighbors coord = List.map offsets ~f:(Coord.add coord)

  let update live =
    let neighbor_counts =
      Set.fold live ~init:Coord.Map.empty ~f:(fun neighbor_counts coord ->
          List.fold
            (neighbors coord)
            ~init:neighbor_counts
            ~f:(fun neighbor_counts neighbor ->
              Map.update neighbor_counts neighbor ~f:(function
                  | None -> 1
                  | Some n -> n + 1)))
    in
    let interesting_coords = Set.union live (Map.key_set neighbor_counts) in
    Set.fold interesting_coords ~init:Coord.Set.empty ~f:(fun next_t coord ->
        let neighbor_count = Option.value (Map.find neighbor_counts coord) ~default:0 in
        let alive_next =
          match Set.mem live coord, neighbor_count with
          | true, (2 | 3) -> true
          | true, _ -> false
          | false, 3 -> true
          | false, _ -> false
        in
        match alive_next with
        | false -> next_t
        | true -> Set.add next_t coord)
  ;;

  let solve input = Set.length (Fn.apply_n_times update ~n:6 input)
end

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 112 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01) ]
