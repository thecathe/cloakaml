(** @canonical Game.Mode.Spec.Roles.Role *)

module type S = sig
  include Enum_map.S

  type kind
  type alignment

  val kind : t -> kind
  val is_kind : kind -> t -> bool
  val alignment : t -> alignment
  val is_alignment : alignment -> t -> bool
end

module type InputS = sig
  include Enum_map.InputS

  type kind
  type alignment

  val kind : t -> kind
end

module Make
    (A : Alignment.S)
    (K : Kind.S with type alignment = A.t)
    (X : InputS with type kind = K.t and type alignment = A.t) :
  S with type t = X.t and type kind = K.t and type alignment = A.t = struct
  include Enum_map.Make (X)

  type kind = K.t
  type alignment = A.t

  let kind : t -> kind = X.kind
  let is_kind (a : kind) (x : t) : bool = kind x |> K.equal a
  let alignment (x : t) : alignment = kind x |> K.alignment
  let is_alignment (a : alignment) (x : t) : bool = alignment x |> A.equal a
end
