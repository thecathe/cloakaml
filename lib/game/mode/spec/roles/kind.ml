(** @canonical Game.Mode.Spec.Role.Kind *)

module type S = sig
  include Enum_map.S

  type alignment

  val alignment : t -> alignment
  val is_alignment : t -> alignment -> bool
end

module type InputS = sig
  include Enum_map.InputS

  type alignment

  val alignment : t -> alignment
end

module Make (A : Alignment.S) (X : InputS with type alignment = A.t) :
  S with type t = X.t and type alignment = A.t = struct
  include Enum_map.Make (X)

  type alignment = A.t

  let alignment = X.alignment
  let is_alignment (x : t) (b : alignment) : bool = alignment x |> A.equal b
end
