open! Core
open! Import

module Common = struct
  module Input = Input.Make_parseable (struct
    type t = int list

    let parser = Angstrom.(sep_by (char ',') parse_int)
  end)

  module Output = Int
end

let test_case = "0,3,6"

module Part_01 = struct
  include Common

  let solve input =
    let seen = Int.Table.create () in
    let add_number index number =
      Hashtbl.update seen number ~f:(function
          | None -> [ index ]
          | Some l -> index :: l)
    in
    List.iteri input ~f:(fun index number ->
        let index = index + 1 in
        add_number index number);
    let rec loop index last =
      let next_number =
        match Hashtbl.find seen last with
        | Some [ n ] ->
          assert (n = index - 1);
          0
        | Some (m :: n :: _) ->
          assert (m = index - 1);
          m - n
        | _ -> assert false
      in
      match index with
      | 2020 -> next_number
      | _ ->
        add_number index next_number;
        loop (index + 1) next_number
    in
    loop (List.length input + 1) (List.last_exn input)
  ;;
end

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 436 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01) ]
