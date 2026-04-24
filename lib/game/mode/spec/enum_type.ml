(** @canonical Game.Mode.Spec.EnumType *)

(** {1 unused} *)

module type S = sig
  type t [@@deriving show { with_path = false }, eq, enum]
end

module Make (X : S) : S with type t = X.t = struct
  include X
end
