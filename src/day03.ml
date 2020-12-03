open! Core
open! Import

module Grid = struct
  module T = struct
    type t =
      { trees : Int_pair.Hash_set.t
      ; width : int
      ; height : int
      }

    let contains { trees; width; height } ~row ~col =
      (match row > height with
      | false -> ()
      | true -> raise_s [%message "Row is too high" (row : int) (height : int)]);
      let modded_col = col % width in
      Hash_set.mem trees (row, modded_col)
    ;;

    let parser =
      let open Angstrom in
      let one_row =
        scan_state (0, []) (fun (index, trees) char ->
            match char with
            | '.' -> Some (index + 1, trees)
            | '#' -> Some (index + 1, index :: trees)
            | _ -> None)
      in
      let all_rows = sep_by end_of_line one_row in
      map all_rows ~f:(fun rows ->
          let rows = List.filter rows ~f:(fun (length, _trees) -> length <> 0) in
          let width =
            List.map rows ~f:fst
            |> List.reduce_exn ~f:(fun a b ->
                   match a = b with
                   | true -> a
                   | false ->
                     raise_s [%message "Rows have an uneven width" (a : int) (b : int)])
          in
          let height = List.length rows in
          let trees = Int_pair.Hash_set.create () in
          List.iteri rows ~f:(fun row (_length, cols) ->
              List.iter cols ~f:(fun col -> Hash_set.add trees (row, col)));
          { trees; width; height })
    ;;
  end

  include T
  include Input.Make_parseable (T)
end

module Common = struct
  module Input = Input.Make_parseable (Grid)
  module Output = Int

  let test_case =
    {|..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#|}
  ;;
end

module Part_01 = struct
  include Common

  let count (grid : Grid.t) ~drow ~dcol =
    let cells =
      Sequence.unfold ~init:(0, 0) ~f:(fun v ->
          let new_ = Int_pair.add v (drow, dcol) in
          Some (new_, new_))
    in
    Sequence.take_while cells ~f:(fun (row, _col) -> row < grid.height)
    |> Sequence.count ~f:(fun (row, col) -> Grid.contains grid ~row ~col)
  ;;

  let solve = count ~drow:1 ~dcol:3

  let%expect_test _ =
    Input.of_string test_case |> solve |> printf "%d\n";
    [%expect {| 7 |}]
  ;;
end

module Part_02 = struct
  include Common

  let solve grid =
    let counts =
      List.map [ 1, 1; 1, 3; 1, 5; 1, 7; 2, 1 ] ~f:(fun (drow, dcol) ->
          Part_01.count grid ~drow ~dcol)
    in
    List.reduce_exn counts ~f:( * )
  ;;

  let%expect_test _ =
    Input.of_string test_case |> solve |> printf "%d\n";
    [%expect {| 336 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
