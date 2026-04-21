(** @canonical Game.Meta.Players.Map *)

open Player
include Hashtbl.Make (Player)
