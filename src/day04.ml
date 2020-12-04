open! Core
open! Import

module Passport = struct
  type t = string String.Map.t [@@deriving sexp]

  let parser =
    let open Angstrom in
    let field =
      both (take_while Char.is_alpha <* char ':') (take_while (Fn.non Char.is_whitespace))
      <?> "field"
    in
    sep_by1 (skip Char.is_whitespace) field >>| String.Map.of_alist_exn
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (struct
    type t = Passport.t list

    let parser =
      Angstrom.(
        sep_by (end_of_line *> end_of_line) Passport.parser
        <* (end_of_line <|> end_of_input))
    ;;
  end)

  module Output = Int

  let test_case =
    {|ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
|}
  ;;
end

module Part_01 = struct
  include Common

  let required_fields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
  let is_valid passport = List.for_all required_fields ~f:(Map.mem passport)
  let solve = List.count ~f:is_valid

  let%expect_test _ =
    let input = Input.of_string test_case in
    print_s [%sexp (input : Passport.t list)];
    [%expect
      {|
      (((byr 1937) (cid 147) (ecl gry) (eyr 2020) (hcl #fffffd) (hgt 183cm)
        (iyr 2017) (pid 860033327))
       ((byr 1929) (cid 350) (ecl amb) (eyr 2023) (hcl #cfa07d) (iyr 2013)
        (pid 028048884))
       ((byr 1931) (ecl brn) (eyr 2024) (hcl #ae17e1) (hgt 179cm) (iyr 2013)
        (pid 760753108))
       ((ecl brn) (eyr 2025) (hcl #cfa07d) (hgt 59in) (iyr 2011) (pid 166559648))) |}];
    let input = Input.of_string test_case in
    print_s [%sexp (solve input : int)];
    [%expect {| 2 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
