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
end

module Part_01 = struct
  include Common

  let required_fields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
  let is_valid passport = List.for_all required_fields ~f:(Map.mem passport)
  let solve = List.count ~f:is_valid

  let%expect_test _ =
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
    in
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
    print_s [%sexp (solve input : int)];
    [%expect {| 2 |}]
  ;;
end

module Part_02 = struct
  include Common
  open Angstrom

  let true_or_fail =
    bind ~f:(function
        | true -> return ()
        | false -> fail "n")
  ;;

  let validate_as_number ~validate =
    take_while Char.is_digit >>| Int.of_string >>| validate |> true_or_fail
  ;;

  let in_range lower upper n = lower <= n && n <= upper

  let height =
    map2
      (take_while Char.is_digit >>| Int.of_string)
      (choice
         [ string "cm" *> return (in_range 150 193)
         ; string "in" *> return (in_range 59 76)
         ])
      ~f:(fun x f -> f x)
    |> true_or_fail
  ;;

  let hair_color =
    char '#'
    *> count
         6
         (skip (function
             | '0' .. '9' | 'a' .. 'f' -> true
             | _ -> false))
    *> return ()
  ;;

  let eye_color =
    choice (List.map [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ] ~f:string)
    *> return ()
  ;;

  let passport_id = count 9 (skip Char.is_digit) *> return ()

  let rules =
    [ "byr", validate_as_number ~validate:(in_range 1920 2002)
    ; "iyr", validate_as_number ~validate:(in_range 2010 2020)
    ; "eyr", validate_as_number ~validate:(in_range 2020 2030)
    ; "hgt", height
    ; "hcl", hair_color
    ; "ecl", eye_color
    ; "pid", passport_id
    ]
  ;;

  let is_valid passport =
    List.for_all rules ~f:(fun (key, rule) ->
        Map.find passport key
        |> Option.value_map ~default:false ~f:(fun data ->
               Angstrom.parse_string ~consume:All rule data |> Result.is_ok))
  ;;

  let solve = List.count ~f:is_valid

  let%expect_test "invalid" =
    let test_case =
      {|eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
|}
    in
    let input = Input.of_string test_case in
    assert (List.for_all input ~f:(Fn.non is_valid))
  ;;

  let%expect_test "valid" =
    let test_case =
      {|pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
|}
    in
    let input = Input.of_string test_case in
    assert (List.for_all input ~f:is_valid)
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
