open! Core
open! Import

module Instruction = struct
  module Operation = struct
    type t =
      | Accumulate
      | Jump
      | No_op
    [@@deriving sexp]

    let of_string = function
      | "acc" -> Accumulate
      | "jmp" -> Jump
      | "nop" -> No_op
      | s -> raise_s [%message "Invalid operation string" (s : string)]
    ;;
  end

  type t =
    { operation : Operation.t
    ; argument : int
    }
  [@@deriving sexp]

  open Angstrom

  let operation = take_while1 Char.is_alpha >>| Operation.of_string <?> "operation"

  let plus_or_minus =
    any_char
    >>| (function
          | '+' -> ident
          | '-' -> ( ~- )
          | c -> raise_s [%message "Unrecognized argument sign" (c : char)])
    <?> "plus_or_minus"
  ;;

  let argument = plus_or_minus <*> parse_int

  let parser =
    map2
      (operation <* char ' ')
      argument
      ~f:(fun operation argument -> { operation; argument })
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Instruction)
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve input =
    let program = List.to_array input in
    let visited = Int.Hash_set.create () in
    let rec loop ~index ~accumulator =
      match Hash_set.mem visited index with
      | true -> accumulator
      | false ->
        Hash_set.add visited index;
        let ({ operation; argument } : Instruction.t) = program.(index) in
        (match operation with
        | Accumulate -> loop ~index:(index + 1) ~accumulator:(accumulator + argument)
        | Jump -> loop ~index:(index + argument) ~accumulator
        | No_op -> loop ~index:(index + 1) ~accumulator)
    in
    loop ~index:0 ~accumulator:0
  ;;
end

let%expect_test _ =
  let test_case = {|nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6|} in
  Part_01.Input.of_string test_case |> Part_01.solve |> printf "%d\n";
  [%expect {| 5 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01) ]
