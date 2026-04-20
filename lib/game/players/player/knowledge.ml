(** @canonical Game.Players.Player.Knowledge *)

type t =
  | Just of data
  | Either of data * data

and data = PlayerIs of int * Roles.Group.t
