(** @canonical Cloakaml.Game.Spec.Role *)

module Alignment = Alignment
module Kind = Kind

module type S = sig
  include Enum_map.S
  module Kind : Kind.S

  type kind = Kind.t
  type alignment = Kind.alignment

  val equal_kind : kind -> kind -> bool
  val equal_alignment : alignment -> alignment -> bool
  val kind : t -> kind
  val is_kind : kind -> t -> bool
  val alignment : t -> alignment
  val is_alignment : alignment -> t -> bool
end

module type InputS = sig
  include Enum_map.InputS

  type kind

  val kind : t -> kind
end

module Make (K : Kind.S) (X : InputS with type kind = K.t) :
  S with type t = X.t and module Kind = K and type kind = K.t = struct
  include Enum_map.Make (X)
  module Kind = K

  type kind = K.t
  type alignment = K.alignment

  let equal_kind : kind -> kind -> bool = K.equal
  let equal_alignment : alignment -> alignment -> bool = K.equal_alignment
  let kind : t -> kind = X.kind
  let is_kind (a : kind) (x : t) : bool = kind x |> equal_kind a
  let alignment (x : t) : alignment = kind x |> K.alignment

  let is_alignment (a : alignment) (x : t) : bool =
    alignment x |> equal_alignment a
  ;;
end
