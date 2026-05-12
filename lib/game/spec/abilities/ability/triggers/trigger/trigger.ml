(** @canonical Game.Spec.Abilities.Ability.Triggers.Trigger *)

module Kind = Kind

module type S = sig
  include Enum_map.S

  module Kind : Kind.S
  type kind = Kind.t

  val kind : t -> kind
  val is_kind : kind -> t -> bool
  val is_kinds : kind list -> t -> bool
end

module type InputS = sig
  include Enum_map.InputS

  type kind

  val kind : t -> kind
end

module Make (K : Kind.S) (X : InputS with type kind = K.t) :
  S with type t = X.t and module Kind = K = struct
  include Enum_map.Make (X)

  module Kind = K
  type kind = K.t

  let kind = X.kind
  let is_kind (a : kind) (x : t) : bool = kind x |> K.equal a

  (** returns disjunction of {!fun:is_kind} over [xs] *)
  let is_kinds (xs : kind list) (y : t) : bool =
    List.exists (fun x -> is_kind x y) xs
  ;;
end
