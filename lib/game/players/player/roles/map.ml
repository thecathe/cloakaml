(** @canonical Game.Players.Player.Roles.Map *)

open Role

include Hashtbl.Make (Role) (** @closed *)
