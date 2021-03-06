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
  end

  include T
  include Input.Make_parseable (T)
end

module Common = struct
  module Input = Input.Make_parseable_many (Candidate)
  module Output = Int

  let test_cases = [ "1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc" ]
end

module Part_01 = struct
  include Common

  let is_valid ({ policy = { bounds = { lower; upper }; char }; password } : Candidate.t) =
    Maybe_bound.interval_contains_exn
      ~lower:(Incl lower)
      ~upper:(Incl upper)
      ~compare
      (String.count password ~f:(Char.equal char))
  ;;

  let solve = List.count ~f:is_valid

  let%expect_test _ =
    List.iter test_cases ~f:(fun case ->
        print_s
          [%sexp { case : string; is_valid : bool = is_valid (Candidate.of_string case) }]);
    [%expect
      {|
      ((case "1-3 a: abcde") (is_valid true))
      ((case "1-3 b: cdefg") (is_valid false))
      ((case "2-9 c: ccccccccc") (is_valid true)) |}]
  ;;
end

module Part_02 = struct
  include Common

  let is_valid ({ policy = { bounds = { lower; upper }; char }; password } : Candidate.t) =
    List.count [ lower; upper ] ~f:(fun index ->
        let char_by_one_based_index = password.[index - 1] in
        Char.equal char char_by_one_based_index)
    |> equal 1
  ;;

  let solve = List.count ~f:is_valid

  let%expect_test _ =
    List.iter test_cases ~f:(fun case ->
        print_s
          [%sexp { case : string; is_valid : bool = is_valid (Candidate.of_string case) }]);
    [%expect
      {|
      ((case "1-3 a: abcde") (is_valid true))
      ((case "1-3 b: cdefg") (is_valid false))
      ((case "2-9 c: ccccccccc") (is_valid false)) |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
