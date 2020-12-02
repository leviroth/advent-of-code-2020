open! Core
open! Import

module type Output = sig
  type t

  val to_string : t -> string
end

module type Part = sig
  module Input : Input.S
  module Output : Output

  val one_based_index : int
  val solve : Input.t -> Output.t
end

module type Day = sig
  val day_of_month : int
  val parts : (module Part) list
end

module type Solution = sig
  module type Output = Output
  module type Part = Part
  module type Day = Day
end
