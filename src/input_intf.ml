open! Core

module type S = sig
  type t

  val of_string : string -> t
  val load : string -> t
end

module type Input = sig
  module type S = S

  module Int_list : S with type t = int list
  module String_list : S with type t = string list
  module String : S with type t = string
end
