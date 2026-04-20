(** @canonical Game.Players.Player.Roles.Group *)

type t =
  | Role of Role.t
  | Kind of Role.Kind.t
  | Alignment of Role.Alignment.t
[@@deriving show { with_path = false }, eq]
