open! Core
open! Import

module Coord2 = struct
  module T = struct
    type t = int * int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let zero = 0, 0, 0
  let ( + ) (a, b, c) (a', b', c') = a + a', b + b', c + c'

  let cartesian_product l =
    let open List.Let_syntax in
    let%bind x = l
    and y = l
    and z = l in
    [ x, y, z ]
  ;;

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
        |> Set.of_list)
  ;;
end

let test_case = {|.#.
..#
###|}

module Solve_gen (Coord : sig
  type t

  include Comparable.S with type t := t
  include Container.Summable with type t := t

  val cartesian_product : int list -> t list
  val parser : Set.t Angstrom.t
end) =
struct
  module Input = Input.Make_parseable (struct
    include Coord.Set

    let parser = Coord.parser
  end)

  module Output = Int

  let offsets =
    let range = [ -1; 0; 1 ] in
    Set.remove (Coord.Set.of_list (Coord.cartesian_product range)) Coord.zero
    |> Set.to_list
  ;;

  let neighbors coord = List.map offsets ~f:(Coord.( + ) coord)

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

module Part_01 = Solve_gen (Coord2)

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 112 |}]
;;

module Coord3 = struct
  module T = struct
    type t = int * int * int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let zero = 0, 0, 0, 0
  let ( + ) (a, b, c, d) (a', b', c', d') = a + a', b + b', c + c', d + d'

  let cartesian_product l =
    let open List.Let_syntax in
    let%bind x = l
    and y = l
    and z = l
    and w = l in
    [ x, y, z, w ]
  ;;

  let parser = Angstrom.map Coord2.parser ~f:(Set.map ~f:(fun (a, b, c) -> a, b, c, 0))
end

module Part_02 = Solve_gen (Coord3)

let%expect_test _ =
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve input : int)];
  [%expect {| 848 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
