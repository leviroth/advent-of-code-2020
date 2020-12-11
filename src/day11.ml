open! Core
open! Import

module Cell = struct
  type t =
    | Floor
    | Empty_seat
    | Occupied_seat
  [@@deriving sexp, equal]

  let parser =
    let open Angstrom in
    any_char
    >>= function
    | '.' -> return Floor
    | 'L' -> return Empty_seat
    | '#' -> return Occupied_seat
    | c -> fail (sprintf "Unexpected char '%c'" c)
  ;;
end

module Grid = struct
  type t = Cell.t Int_pair.Map.t [@@deriving sexp]

  let parser =
    let open Angstrom in
    let one_row = many1 Cell.parser <* take_while Char.is_whitespace in
    let all = many one_row in
    map all ~f:(fun all ->
        List.mapi all ~f:(fun row_num row ->
            List.mapi row ~f:(fun col_num cell -> (row_num, col_num), cell))
        |> List.concat
        |> Int_pair.Map.of_alist_exn)
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Grid)
  module Output = Int
end

module Make (M : sig
  val tolerance : int
  val neighbors : Grid.t -> coords:int * int -> Cell.t list
end) =
struct
  include Common

  let new_cell (t : Grid.t) ~coords : Cell.t option =
    match Map.find_exn t coords with
    | Floor -> None
    | Empty_seat ->
      (match List.exists (M.neighbors t ~coords) ~f:(Cell.equal Occupied_seat) with
      | true -> None
      | false -> Some Occupied_seat)
    | Occupied_seat ->
      (match
         List.count (M.neighbors t ~coords) ~f:(Cell.equal Occupied_seat) >= M.tolerance
       with
      | true -> Some Empty_seat
      | false -> None)
  ;;

  let update t =
    Map.fold t ~init:Int_pair.Map.empty ~f:(fun ~key:coords ~data:cell new_t ->
        let cell = Option.value (new_cell t ~coords) ~default:cell in
        Map.set new_t ~key:coords ~data:cell)
  ;;

  let solve input =
    let rec loop grid =
      let new_grid = update grid in
      match [%equal: Cell.t Int_pair.Map.t] grid new_grid with
      | true -> grid
      | false -> loop new_grid
    in
    Map.count (loop input) ~f:(Cell.equal Occupied_seat)
  ;;
end

let offsets =
  let range = List.range (-1) 2 in
  let all = List.cartesian_product range range |> Int_pair.Set.of_list in
  Set.remove all (0, 0) |> Set.to_list
;;

let%expect_test _ =
  print_s [%sexp (offsets : (int * int) list)];
  [%expect {| ((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) |}]
;;

module Part_01 = Make (struct
  let tolerance = 4
  let surrounding_coords coords = List.map offsets ~f:(Int_pair.add coords)

  let neighbors t ~coords =
    let surrounding_coords = surrounding_coords coords in
    List.filter_map surrounding_coords ~f:(Map.find t)
  ;;
end)

let test_case =
  {|L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL|}
;;

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 37 |}]
;;

module Part_02 = Make (struct
  let tolerance = 5

  let neighbors (t : Grid.t) ~coords =
    List.filter_map offsets ~f:(fun offset ->
        let rec loop coords =
          match Map.find t coords with
          | (None | Some (Empty_seat | Occupied_seat)) as v -> v
          | Some Floor -> loop (Int_pair.add coords offset)
        in
        loop (Int_pair.add coords offset))
  ;;
end)

let%expect_test _ =
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve input : int)];
  [%expect {| 26 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
