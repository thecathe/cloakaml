(** @canonical Cloakaml.Game.Mode.Spec.Player.Status *)

module type S = sig
  include Enum_map.S

  val initial : t
end

module type InputS = sig
  include Enum_map.InputS

  val initial : t
end

module Make (X : InputS) : S with type t = X.t = struct
  include Enum_map.Make (X)

  let initial : t = X.initial
end
