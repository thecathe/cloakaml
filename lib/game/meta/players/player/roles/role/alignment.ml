(** @canonical Game.Meta.Players.Player.Roles.Role.Alignment *)

type t =
  | Good
  | Evil
[@@deriving show { with_path = false }, eq]
