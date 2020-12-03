open! Core

module type S = sig
  type t

  val of_string : string -> t
  val load : string -> t
end

module type Parser = sig
  type t

  val parser : t Angstrom.t
end

module type Input = sig
  module type S = S
  module type Parser = Parser

  module Make_parseable (Parser : Parser) : S with type t = Parser.t
  module Make_parseable_many (Parser : Parser) : S with type t = Parser.t list
  module Int_list : S with type t = int list
  module String_list : S with type t = string list
  module String : S with type t = string
end
