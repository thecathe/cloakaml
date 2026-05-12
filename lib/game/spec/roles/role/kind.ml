(** @canonical Cloakaml.Game.Spec.Role.Kind *)

module type S = sig
  include Enum_map.S
  module Alignment : Alignment.S

  type alignment = Alignment.t

  val equal_alignment : alignment -> alignment -> bool
  val alignment : t -> alignment
  val is_alignment : alignment -> t -> bool
end

module type InputS = sig
  include Enum_map.InputS

  type alignment

  val alignment : t -> alignment
end

module Make (A : Alignment.S) (X : InputS with type alignment = A.t) :
  S with type t = X.t and module Alignment = A and type alignment = A.t = struct
  include Enum_map.Make (X)
  module Alignment = A

  type alignment = Alignment.t

  let equal_alignment = Alignment.equal
  let alignment = X.alignment

  let is_alignment (a : alignment) (x : t) : bool =
    alignment x |> equal_alignment a
  ;;
end
