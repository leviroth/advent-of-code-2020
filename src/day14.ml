open! Core
open! Import

let test_case =
  {|mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0|}
;;

module Mask = struct
  type t =
    { and_ : int
    ; or_ : int
    }
  [@@deriving sexp]

  let get_positions_from_right string char =
    String.fold string ~init:0 ~f:(fun acc c ->
        let acc = acc lsl 1 in
        match Char.equal char c with
        | true -> acc + 1
        | false -> acc)
  ;;

  let of_string string =
    let and_ = lnot (get_positions_from_right string '0') in
    let or_ = get_positions_from_right string '1' in
    { and_; or_ }
  ;;
end

module Instruction = struct
  type t =
    | Set_mask of Mask.t
    | Set_memory of
        { index : int
        ; value : int
        }
  [@@deriving sexp]

  open Angstrom

  let separator = string " = " *> return ()

  let mask =
    map
      (string "mask" *> separator *> take_till Char.is_whitespace)
      ~f:(fun s -> Set_mask (Mask.of_string s))
  ;;

  let mem =
    map2
      (string "mem[" *> parse_int <* char ']' <* separator)
      parse_int
      ~f:(fun index value -> Set_memory { index; value })
  ;;

  let parser = mask <|> mem
end

module Common = struct
  module Input = Input.Make_parseable_many (Instruction)
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve (input : Input.t) =
    let memory = Int.Table.create () in
    let mask = ref ({ and_ = 0; or_ = 0 } : Mask.t) in
    List.iter input ~f:(fun instruction ->
        match instruction with
        | Set_mask mask' -> mask := mask'
        | Set_memory { index; value } ->
          let ({ and_; or_ } : Mask.t) = !mask in
          let value = value land and_ lor or_ in
          Hashtbl.set memory ~key:index ~data:value);
    List.sum (module Int) (Hashtbl.data memory) ~f:ident
  ;;
end

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 165 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01) ]
