open! Core
open! Import

module Set_instruction = struct
  type t =
    { address : int
    ; value : int
    }
  [@@deriving sexp]
end

module type Mask = sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
  val apply : t -> Set_instruction.t -> Set_instruction.t list
end

module Instruction (Mask : Mask) = struct
  type t =
    | Set_mask of Mask.t
    | Set_memory of Set_instruction.t
  [@@deriving sexp_of]

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
      ~f:(fun address value -> Set_memory { address; value })
  ;;

  let parser = mask <|> mem
end

module Solve_gen (Mask : Mask) = struct
  module Instruction = Instruction (Mask)
  module Input = Input.Make_parseable_many (Instruction)
  module Output = Int

  let solve (input : Input.t) =
    let memory = Int.Table.create () in
    let mask = ref None in
    List.iter input ~f:(fun instruction ->
        match instruction with
        | Set_mask mask' -> mask := Some mask'
        | Set_memory instruction ->
          let mask = Option.value_exn !mask in
          let instructions = Mask.apply mask instruction in
          List.iter instructions ~f:(fun { address; value } ->
              Hashtbl.set memory ~key:address ~data:value));
    List.sum (module Int) (Hashtbl.data memory) ~f:ident
  ;;
end

module Part_1_mask = struct
  type t =
    { and_ : int
    ; or_ : int
    }
  [@@deriving sexp_of]

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

  let apply { and_; or_ } ({ address; value } : Set_instruction.t)
      : Set_instruction.t list
    =
    [ { address; value = value land and_ lor or_ } ]
  ;;
end

module Part_01 = Solve_gen (Part_1_mask)

let%expect_test _ =
  let test_case =
    {|mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0|}
  in
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 165 |}]
;;

module Part_2_mask = struct
  type t = (int -> int) list list [@@deriving sexp_of]

  let of_string string =
    String.to_list string
    |> List.rev
    |> List.mapi ~f:(fun i c ->
           match c with
           | '0' -> [ ident ]
           | '1' ->
             let mask = 1 lsl i in
             [ (fun n -> n lor mask) ]
           | 'X' ->
             let or_mask = 1 lsl i in
             let and_mask = lnot or_mask in
             [ (fun n -> n lor or_mask); (fun n -> n land and_mask) ]
           | _ -> assert false)
  ;;

  let addresses t base_address =
    List.fold t ~init:[ base_address ] ~f:(fun ns fs ->
        let open List.Let_syntax in
        let%bind f = fs in
        let%bind n = ns in
        [ f n ])
  ;;

  let%expect_test _ =
    let t = of_string "000000000000000000000000000000X1001X" in
    print_s [%sexp (addresses t 42 : int list)];
    [%expect {| (59 58 27 26) |}]
  ;;

  let apply t ({ address; value } : Set_instruction.t) =
    let addresses = addresses t address in
    List.map addresses ~f:(fun address -> ({ address; value } : Set_instruction.t))
  ;;
end

module Part_02 = Solve_gen (Part_2_mask)

let%expect_test _ =
  let test_case =
    {|mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1|}
  in
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve input : int)];
  [%expect {| 208 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
