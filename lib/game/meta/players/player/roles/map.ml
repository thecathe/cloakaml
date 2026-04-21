(** @canonical Game.Meta.Players.Player.Roles.Map *)

open Role

include Hashtbl.Make (Role) (** @closed *)
