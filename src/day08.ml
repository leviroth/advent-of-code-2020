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

let run program =
  let visited = Int.Hash_set.create () in
  let rec loop ~index ~accumulator =
    match Hash_set.mem visited index with
    | true -> `Looped accumulator
    | false ->
      Hash_set.add visited index;
      (match program.(index) with
      | exception Invalid_argument _ -> `Terminated accumulator
      | ({ operation; argument } : Instruction.t) ->
        (match operation with
        | Accumulate -> loop ~index:(index + 1) ~accumulator:(accumulator + argument)
        | Jump -> loop ~index:(index + argument) ~accumulator
        | No_op -> loop ~index:(index + 1) ~accumulator))
  in
  loop ~index:0 ~accumulator:0
;;

module Part_01 = struct
  include Common

  let solve input =
    let program = List.to_array input in
    match run program with
    | `Looped accumulator -> accumulator
    | `Terminated _ -> failwith "Unexpected termination"
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

module Part_02 = struct
  include Common

  let solve (input : Input.t) =
    let run_with_modified_operation index operation =
      let program = List.to_array input in
      program.(index) <- { (program.(index)) with operation };
      match run program with
      | `Looped _ -> None
      | `Terminated accumulator -> Some accumulator
    in
    List.find_mapi_exn input ~f:(fun index instruction ->
        match instruction.operation with
        | Accumulate -> None
        | Jump -> run_with_modified_operation index No_op
        | No_op -> run_with_modified_operation index Jump)
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
  Part_02.Input.of_string test_case |> Part_02.solve |> printf "%d\n";
  [%expect {| 8 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
