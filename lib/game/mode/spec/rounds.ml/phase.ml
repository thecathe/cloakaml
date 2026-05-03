(** @canonical Game.Mode.Spec.Rounds.Phase *)

module type S = sig
  include Enum_map.S

  val step : ?starting:t -> int -> t
end

module type InputS = sig
  include Enum_map.InputS

  val starting : t
end

module Make (X : InputS) : S with type t = X.t = struct
  include Enum_map.Make (X)

  let step ?(starting : t = X.starting) (n : int) : t =
    next ~wrap:true ~amount:n starting
  ;;
end
