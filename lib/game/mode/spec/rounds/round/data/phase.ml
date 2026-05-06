(** @canonical Game.Mode.Spec.Round.Data.Phase *)

module type S = sig
  include Enum_map.S

  val initial : t
  val step : ?starting:t -> int -> t
end

module type InputS = sig
  include Enum_map.InputS

  val initial : t
end

module Make (X : InputS) : S with type t = X.t = struct
  include Enum_map.Make (X)

  let initial = X.initial

  let step ?(starting : t = X.initial) (n : int) : t =
    next ~wrap:true ~amount:n starting
  ;;
end
