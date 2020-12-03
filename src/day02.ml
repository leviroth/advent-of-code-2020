open! Core
open! Import

module Policy = struct
  module Bounds = struct
    type t =
      { lower : int
      ; upper : int
      }
    [@@deriving sexp]

    let parser =
      let open Angstrom in
      lift2 (fun lower upper -> { lower; upper }) parse_int (char '-' *> parse_int)
    ;;
  end

  type t =
    { bounds : Bounds.t
    ; char : char
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    lift2 (fun bounds char -> { bounds; char }) Bounds.parser (char ' ' *> any_char)
  ;;
end

module Candidate = struct
  module T = struct
    type t =
      { policy : Policy.t
      ; password : string
      }
    [@@deriving sexp]

    let parser =
      let open Angstrom in
      lift2
        (fun policy password -> { policy; password })
        Policy.parser
        (string ": " *> take_while1 Char.is_alpha)
    ;;

    let is_valid_part_1 { policy = { bounds = { lower; upper }; char }; password } =
      Maybe_bound.interval_contains_exn
        ~lower:(Incl lower)
        ~upper:(Incl upper)
        ~compare
        (String.count password ~f:(Char.equal char))
    ;;

    let is_valid_part_2 { policy = { bounds = { lower; upper }; char }; password } =
      List.count [ lower; upper ] ~f:(fun index ->
          let char_by_one_based_index = password.[index - 1] in
          Char.equal char char_by_one_based_index)
      |> equal 1
    ;;
  end

  include T
end

module Common = struct
  module Input = Input.Make_parseable_many (Candidate)
  module Output = Int

  let test_case = {|1-3 a: abcde
  1-3 b: cdefg
  2-9 c: ccccccccc|}
end

module Part_01 = struct
  include Common

  let one_based_index = 1
  let solve = List.count ~f:Candidate.is_valid_part_1

  let%expect_test _ =
    Input.of_string test_case |> solve |> printf "%d\n";
    [%expect {| 2 |}]
  ;;
end

module Part_02 = struct
  include Common

  let one_based_index = 2
  let solve = List.count ~f:Candidate.is_valid_part_2

  let%expect_test _ =
    Input.of_string test_case |> solve |> printf "%d\n";
    [%expect {| 1 |}]
  ;;
end

let day_of_month = 2
let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
