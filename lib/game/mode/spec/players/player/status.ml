(** @canonical Cloakaml.Game.Mode.Spec.Player.Status *)

module type S = sig
  include Term.S

  val initial : t
end

module type InputS = sig
  include Term.InputS

  val initial : t
end

module Make (X : InputS) : S with type t = X.t = struct
  include Term.Make (X)

  let initial : t = X.initial
end
