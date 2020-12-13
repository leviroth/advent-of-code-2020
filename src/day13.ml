open! Core
open! Import

let test_case = {|939
        7,13,x,x,59,x,31,19|}

module Notes = struct
  type t =
    { earliest_timestamp : int
    ; buses : int option list
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    let bus = choice [ parse_int >>| Option.some; char 'x' *> return None ] in
    let buses = sep_by (char ',') bus in
    map2
      (parse_int <* take_while Char.is_whitespace)
      buses
      ~f:(fun earliest_timestamp buses -> { earliest_timestamp; buses })
  ;;
end

module Part_01 = struct
  module Input = Input.Make_parseable (Notes)
  module Output = Int

  let solve ({ earliest_timestamp; buses } : Notes.t) =
    let buses = List.filter_opt buses in
    let time_waited bus = bus - (earliest_timestamp % bus) in
    let bus =
      Option.value_exn
        (List.min_elt buses ~compare:(Comparable.lift compare ~f:time_waited))
    in
    bus * time_waited bus
  ;;

  let solver = Angstrom.map Input.parser ~f:solve
end

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 295 |}]
;;

module Part_02 = struct
  module Input = Input.Make_parseable (Notes)
  module Output = Int

  let f n (i, bus) = bus - i = n % bus

  let solve ({ earliest_timestamp = _; buses } : Notes.t) =
    let with_offsets =
      List.filter_mapi buses ~f:(fun i bus -> Option.map bus ~f:(Tuple2.create i))
    in
    let with_target_remainders =
      List.map with_offsets ~f:(fun (i, bus) -> (bus - i) % bus, bus)
      |> List.sort ~compare:(Comparable.reverse [%compare: _ * int])
    in
    let first, rest =
      List.hd_exn with_target_remainders, List.tl_exn with_target_remainders
    in
    List.fold rest ~init:first ~f:(fun (start, step) (remainder, bus) ->
        let start =
          Sequence.unfold ~init:start ~f:(fun start ->
              let v = start + step in
              Some (v, v))
          |> Sequence.find_exn ~f:(fun v -> v % bus = remainder)
        in
        start, bus * step)
    |> fst
  ;;

  let solver = Angstrom.map Input.parser ~f:solve
end

let%expect_test _ =
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve input : int)];
  [%expect {| 1068781 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
