(** @canonical Game.Build.Players.Player.Roles.Map *)

open Role

include Hashtbl.Make (Role) (** @closed *)
