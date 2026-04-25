(** @canonical Game.Build.Abilities.Triggers.Trigger *)

module type S = sig
  include Enum_map.S

  type kind

  val kind : t -> kind
  val is_kind : kind -> t -> bool
end

module type InputS = sig
  include Enum_map.InputS

  type kind

  val kind : t -> kind
end

module Make (K : Kind.S) (X : InputS with type kind = K.t) :
  S with type t = X.t and type kind = K.t = struct
  include Enum_map.Make (X)

  type kind = K.t

  let kind = X.kind
  let is_kind (a : kind) (x : t) : bool = kind x |> K.equal a
end
