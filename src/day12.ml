open! Core
open! Import

module Change = struct
  type t =
    | Move_absolute of Int_pair.t
    | Move_forward of int
    | Rotate of (Int_pair.t -> Int_pair.t)
end

module Instruction = struct
  module Action = struct
    type t =
      | North
      | South
      | East
      | West
      | Left
      | Right
      | Forward
    [@@deriving sexp, equal]

    let parser =
      let open Angstrom in
      map any_char ~f:(function
          | 'N' -> North
          | 'S' -> South
          | 'E' -> East
          | 'W' -> West
          | 'L' -> Left
          | 'R' -> Right
          | 'F' -> Forward
          | c -> raise_s [%message "Unexpected Action.t char" (c : char)])
    ;;
  end

  type t =
    { action : Action.t
    ; value : int
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    map2 Action.parser parse_int ~f:(fun action value -> { action; value })
  ;;

  let rotate_right degrees ((x, y) as vector) =
    match degrees % 360 with
    | 0 | 360 -> vector
    | 90 -> y, -x
    | 180 -> -x, -y
    | 270 -> -y, x
    | _ -> raise_s [%message "Degrees is not a multiple of 90" (degrees : int)]
  ;;

  let rotate_left degrees = rotate_right (360 - degrees)

  let to_change { action; value } : Change.t =
    match action with
    | North -> Move_absolute (0, value)
    | South -> Move_absolute (0, -value)
    | East -> Move_absolute (value, 0)
    | West -> Move_absolute (-value, 0)
    | Forward -> Move_forward value
    | Left -> Rotate (rotate_left value)
    | Right -> Rotate (rotate_right value)
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Instruction)
  module Output = Int
end

module Part_01 = struct
  include Common

  module State = struct
    type t =
      { position : Int_pair.t
      ; direction : Int_pair.t
      }
    [@@deriving sexp]

    let apply t ~(change : Change.t) =
      match change with
      | Move_absolute vector -> { t with position = Int_pair.add t.position vector }
      | Move_forward magnitude ->
        { t with
          position = Int_pair.add t.position (Int_pair.scale t.direction magnitude)
        }
      | Rotate f -> { t with direction = f t.direction }
    ;;
  end

  let solve input =
    let ({ position = x, y; direction = _ } : State.t) =
      List.fold
        input
        ~init:({ position = 0, 0; direction = 1, 0 } : State.t)
        ~f:(fun state instruction ->
          let change = Instruction.to_change instruction in
          State.apply state ~change)
    in
    Int.abs x + Int.abs y
  ;;
end

let test_case = {|F10
N3
F7
R90
F11|}

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 25 |}]
;;

module Part_02 = struct
  include Common

  module State = struct
    type t =
      { position : Int_pair.t
      ; waypoint : Int_pair.t
      }
    [@@deriving sexp]

    let apply t ~(change : Change.t) =
      match change with
      | Move_absolute vector -> { t with waypoint = Int_pair.add t.waypoint vector }
      | Move_forward magnitude ->
        { t with
          position = Int_pair.add t.position (Int_pair.scale t.waypoint magnitude)
        }
      | Rotate f -> { t with waypoint = f t.waypoint }
    ;;
  end

  let solve input =
    let ({ position = x, y; waypoint = _ } : State.t) =
      List.fold
        input
        ~init:({ position = 0, 0; waypoint = 10, 1 } : State.t)
        ~f:(fun state instruction ->
          let change = Instruction.to_change instruction in
          State.apply state ~change)
    in
    Int.abs x + Int.abs y
  ;;
end

let test_case = {|F10
N3
F7
R90
F11|}

let%expect_test _ =
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve input : int)];
  [%expect {| 286 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
