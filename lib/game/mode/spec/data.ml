(** @canonical Game.Mode.Spec.Data *)

module type S = sig
  type t
end

module Make (X : sig
    type t
  end) : S = struct
  type t = X.t
end
