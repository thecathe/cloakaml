(** @canonical Game.Build.Players.Map *)

open Player
include Hashtbl.Make (Player)
