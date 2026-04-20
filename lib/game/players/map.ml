(** @canonical Game.Players.Map *)

open Player
include Hashtbl.Make (Player)
