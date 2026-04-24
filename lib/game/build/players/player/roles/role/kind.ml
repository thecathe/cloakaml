(** @canonical Game.Build.Players.Player.Roles.Role.Kind *)

type t =
  | Townsfolk
  (** a {{!Alignment.Good}Good} character with a {i beneficial} ability. *)
  | Outsider
  (** a {{!Alignment.Good}Good} character with a {i hindering} ability. *)
  | Minion
  (** An {{!Alignment.Evil}Evil} character with an ability that hinders the {{!Alignment.Good}Good} team.
  *)
  | Demon
  (** An {{!Alignment.Evil}Evil} character with the ability to {b kill} at {{!Phase.Night}Night}.
  *)
  | Traveller
  (** May join the game at any time, and may leave the game at any time. *)
  | Fabled (** {i Characters for the {b Storyteller}.} *)
  | Loric (** {i Characters for the {b Storyteller}.} *)
[@@deriving show { with_path = false }, eq]
