(** @canonical Game.Mode.Spec.EnumType *)

(** {1 unused} *)

module type S = sig
  type t [@@deriving show { with_path = false }, eq, enum]

  exception EnumOutOfBounds of int
end

module type InputS = sig
  type t [@@deriving show { with_path = false }, eq, enum]
end

module Make (X : InputS) : S with type t = X.t = struct
  include X

  exception EnumOutOfBounds of int
end
