(** @canonical Cloakaml.Game.Mode.Spec.Roles.Role *)

module type S = sig
  include Enum_map.S
  module Alignment : Alignment.S
  module Kind : Kind.S with type alignment = Alignment.t

  type alignment = Alignment.t
  type kind = Kind.t

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
  (* type alignment *)

  val kind : t -> kind
end

module Make
    (A : Alignment.S)
    (K : Kind.S with type alignment = A.t)
    (X : InputS with type kind = K.t) :
  S
  with type t = X.t
   (* and module Kind = K *)
   (* and type kind = K.t *)
   (* and module Alignment = A *)
   (* and type alignment = A.t *)
    = struct
  include Enum_map.Make (X)
  module Kind = K
  module Alignment = A

  type kind = K.t
  type alignment = A.t

  let equal_kind : kind -> kind -> bool = K.equal
  let equal_alignment : alignment -> alignment -> bool = A.equal
  let kind : t -> kind = X.kind
  let is_kind (a : kind) (x : t) : bool = kind x |> equal_kind a
  let alignment (x : t) : alignment = kind x |> K.alignment

  let is_alignment (a : alignment) (x : t) : bool =
    alignment x |> equal_alignment a
  ;;
end

module Alignment = Alignment
module Kind = Kind
