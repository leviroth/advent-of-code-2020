open! Core
open! Import

module Common = struct
  module Input = Input.Int_list
  module Output = Int

  let solve_gen numbers ~total =
    let set = Hash_set.create (module Int) in
    List.fold_until
      numbers
      ~init:()
      ~finish:(fun () -> None)
      ~f:(fun () n ->
        let diff = total - n in
        match Hash_set.mem set diff with
        | true -> Stop (Some (n * diff))
        | false ->
          Hash_set.add set n;
          Continue ())
  ;;

  let test_case = [ 1721; 979; 366; 299; 675; 1456 ]
end

module Part_01 = struct
  include Common

  let solve numbers = solve_gen numbers ~total:2020 |> Option.value_exn

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 514579 |}]
  ;;
end

module Part_02 = struct
  include Common

  let solve numbers =
    List.find_map numbers ~f:(fun n ->
        Option.map (solve_gen numbers ~total:(2020 - n)) ~f:(( * ) n))
    |> Option.value_exn
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 241861950 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
