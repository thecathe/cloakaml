(** @canonical Cloakaml.Game.Spec.Players.Selector *)

module type S = sig
  type player
  type t = player Utils.Selector.t
end
module Make (P:Player.S): S with type player = P.t and type t = P.t Utils.Selector.t = struct
  type player = P.t
  type t = player Utils.Selector.t
end