open! Core
include Input_intf

module Int_list = struct
  type t = int list [@@deriving sexp]

  let of_string s = String.split_lines s |> List.map ~f:Int.of_string
  let load file = Sexp.load_sexps_conv_exn file Int.t_of_sexp
end

module String_list = struct
  type t = string list [@@deriving sexp]

  let of_string s = String.split_lines s
  let load file = Sexp.load_sexps_conv_exn file String.t_of_sexp
end

module String = struct
  type t = string [@@deriving sexp]

  let of_string s = s
  let load file = Sexp.load_sexp_conv_exn file String.t_of_sexp
end

module Make_parseable (T : Parser) = struct
  type t = T.t

  let of_string s = Angstrom.parse_string T.parser s ~consume:All |> Result.ok_or_failwith

  let load file =
    In_channel.with_file file ~f:(fun in_channel ->
        Angstrom_unix.parse T.parser in_channel |> snd |> Result.ok_or_failwith)
  ;;
end

module Make_parseable_many (T : Parser) = struct
  include Make_parseable (struct
    type t = T.t list

    let parser = Angstrom.sep_by (Angstrom.take_while1 Char.is_whitespace) T.parser
  end)
end