open! Core
open! Import

module Common = struct
  module Input = Input.Make_parseable_many (struct
    type t = int

    let parser =
      let scan_and_shift string ~true_ ~false_ =
        String.fold string ~init:0 ~f:(fun n c ->
            (n lsl 1)
            +
            match Char.equal c true_ with
            | true -> 1
            | false ->
              (match Char.equal c false_ with
              | true -> 0
              | false -> raise_s [%message "Unexpected char" (c : char) (string : string)]))
      in
      let open Angstrom in
      map2 (take 7) (take 3) ~f:(fun row col ->
          (8 * scan_and_shift row ~true_:'B' ~false_:'F')
          + scan_and_shift col ~true_:'R' ~false_:'L')
    ;;
  end)

  module Output = Int
end

module Part_01 = struct
  include Common

  let solve i = List.max_elt i ~compare |> Option.value_exn
end

module Part_02 = struct
  include Common

  let solve i =
    let min = List.min_elt i ~compare |> Option.value_exn in
    let max = List.max_elt i ~compare |> Option.value_exn in
    let set = Int.Hash_set.of_list i in
    List.find_exn (List.range min (max + 1)) ~f:(Fn.non (Hash_set.mem set))
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
