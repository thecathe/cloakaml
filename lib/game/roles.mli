(** {1 Role.alignment} *)

type alignment =
  | Good
  | Evil
[@@deriving show, eq]

(** {1 Role.kind} *)

type kind =
  | Townsfolk (** a {!Good} character with a {i beneficial} ability. *)
  | Outsider (** a {!Good} character with a {i hindering} ability. *)
  | Minion
  (** An {!Evil} character with an ability that hinders the {!Good} team. *)
  | Demon
  (** An {!Evil} character with the ability to {b kill} at {{!Round.Phase.Night}Night}.
  *)
  | Traveller
  (** May join the game at any time, and may leave the game at any time. *)
  | Fabled (** {i Characters for the {b Storyteller}.} *)
  | Loric (** {i Characters for the {b Storyteller}.} *)
[@@deriving show, eq]

(** {1 Role.t} *)

type t =
  (* townsfolk *)
  | Washerwoman
  (** You start knowing that 1 of 2 players is a particular {!Townsfolk}. *)
  | Librarian
  (** You start knowing that 1 of 2 players is a particular {!Outsider} (or that zero are in play).
  *)
  | Investigator
  (** You start knowing that 1 of 2 players is a particular {!Minion}. *)
  | Chef (** You start knowing how many pairs of {!Evil} players there are. *)
  | Empath
  (** Each {{!Round.Phase.Night}Night}, learn how many of your two {{!Neighbours.t}Neighbours} are {!Evil}.
  *)
  | FortuneTeller
  (** Each {{!Round.Phase.Night}Night}, choose 2 players: you learn if either is a {!Demon}. There is a {!Good} player that registers as a {!Demon} to you (i.e., {b red herring}).
  *)
  | Undertaker
  (** Each {{!Round.Phase.Night}Night}, you learn which {{!Roles.t}Role} died by execution the previous {{!Round.Phase.Day}Day}.
  *)
  | Monk
  (** Each {{!Round.Phase.Night}Night}, choose a player (not yourself): they are {b safe} from the {!Demon} tonight.
  *)
  | Ravenkeeper
  (** If you {b die} at {{!Round.Phase.Night}Night}, you are woken to choose a player: you learn their {{!Roles.t}Role}.
  *)
  | Virgin
  (** The first time you are {i nominated}, if the {b nominator} is a {!Townsfolk}, they are {i executed} immediately.
  *)
  | Slayer
  (** Once per game, during the {{!Round.Phase.Day}Day}, publically choose a player: if they are the {!Demon}, they {b die}.
  *)
  | Soldier (** You are {b safe} from the {!Demon}. *)
  | Mayor
  (** If only 3 players live & no execution occurs, your team {b wins}. If you {b die} at {{!Round.Phase.Night}Night}, another player {i might} {b die} instead.
  *)
  (* outsiders *)
  | Butler
  (** Each {{!Round.Phase.Night}Night}, choose a player (not yourself): {i tomorrow}, you may only vote if they are voting too.
  *)
  | Drunk
  (** You do not know you are the drunk, i.e., you think you are a {!Townsfolk} character, but you are not.
  *)
  | Recluse
  (** Tou might register as {!Evil} and as a {!Minion} or {!Demon}, even if {b dead}.
  *)
  | Saint (** If you {b die} by {i execution}, your team {b loses}. *)
  (* minions *)
  | Poisoner
  (** Each {{!Round.Phase.Night}Night}, choose a player: they are {i poisoned} tonight and tomorrow-day. (This {i secretly} removes/negates their ability.)
  *)
  | Spy
  (** Each {{!Round.Phase.Night}Night}, you see the {b grimoire}. You might register as {!Good} and as a {!Townsfolk} or {!Outsider}, even if {b dead}. {i (The {b grimoire} shows the state-of-the-game.)}
  *)
  | ScarletWoman
  (** If there are 5 or more players {b alive} and the {!Demon} dies, you become the {!Demon}. ({!Traveller}s don't count.)
  *)
  | Baron (** There are extra {!Outsider}s in play (+2). *)
  (* demons *)
  | Imp
  (** Each {{!Round.Phase.Night}Night}, choose a player: they {b die}. If you kill yourself this way, a {!Minion} becomes the {!Imp}.
  *)
[@@deriving show, enum, eq]

(** {2 Role properties} *)

val kind : t -> kind
val is_townfolk : t -> bool
val is_outsider : t -> bool
val is_minion : t -> bool
val is_demon : t -> bool
val is_good : t -> bool
val is_evil : t -> bool

(** {2 Role groups} *)

exception RoleEnumOutOfBounds

val roles : t list
val townsfolk : t list
val outsiders : t list
val minions : t list
val demons : t list

(** {2 Role alignment}*)

exception CannotDetermineAlignment of t

val alignment : t -> alignment
val allied : t -> t -> bool
val opposed : t -> t -> bool
