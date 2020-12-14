open! Core
open! Import

let test_case =
  {|mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0|}
;;

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
end

module Instruction (Mask : sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
end) =
struct
  module Mask = Mask

  type t =
    | Set_mask of Mask.t
    | Set_memory of
        { index : int
        ; value : int
        }
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
      ~f:(fun index value -> Set_memory { index; value })
  ;;

  let parser = mask <|> mem
end

module Common = struct
  module Output = Int
end

module Part_01 = struct
  include Common
  module Instruction = Instruction (Part_1_mask)
  module Input = Input.Make_parseable_many (Instruction)
  module Mask = Part_1_mask

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

  let apply (t : t) n =
    List.fold t ~init:[ n ] ~f:(fun ns fs ->
        let open List.Let_syntax in
        let%bind f = fs in
        let%bind n = ns in
        [ f n ])
  ;;

  let%expect_test _ =
    let t = of_string "000000000000000000000000000000X1001X" in
    print_s [%sexp (apply t 42 : int list)];
    [%expect {| (59 58 27 26) |}]
  ;;
end

module Part_02 = struct
  include Common
  module Instruction = Instruction (Part_2_mask)
  module Input = Input.Make_parseable_many (Instruction)
  module Mask = Part_2_mask

  let solve (input : Input.t) =
    let memory = Int.Table.create () in
    let mask = ref [] in
    List.iter input ~f:(fun instruction ->
        match instruction with
        | Set_mask mask' -> mask := mask'
        | Set_memory { index; value } ->
          let addresses = Mask.apply !mask index in
          List.iter addresses ~f:(fun address ->
              Hashtbl.set memory ~key:address ~data:value));
    List.sum (module Int) (Hashtbl.data memory) ~f:ident
  ;;
end

let test_case =
  {|mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1|}
;;

let%expect_test _ =
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve input : int)];
  [%expect {| 208 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
