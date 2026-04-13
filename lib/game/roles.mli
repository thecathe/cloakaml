(** {1 Role.alignment} *)

type alignment =
  | Good
  | Evil
[@@deriving show, eq]

(** {1 Role.kind} *)

type kind =
  | Townsfolk (** a {!alignment.Good} character with a beneficial ability. *)
  | Outsider (** a {!alignment.Good} character with a hindering ability. *)
  | Minion
  (** an {!alignment.Evil} character with an ability that hinders the {!alignment.Good} team.
  *)
  | Demon
  (** an {!alignment.Evil} character with the ability to {b kill} at night. *)
[@@deriving show, eq]

(** {1 Role.t} *)

type t =
  (* townsfolk *)
  | Washerwoman
  (** you start knowing that 1 of 2 players is a particular townsfolk. *)
  | Librarian
  (** you start knowing that 1 of 2 players is a particular outsider (or that zero are in play).
  *)
  | Investigator
  (** you start knowing that 1 of 2 players is a particular minion. *)
  | Chef (** you start knowing how many pairs of evil players there are. *)
  | Empath (** each night, learn how many of your two neighbours are evil. *)
  | FortuneTeller
  (** each night, choose 2 players: you learn if either is a demon. there is a good player that registers as a demon to you (i.e., {b red herring}).
  *)
  | Undertaker
  (** each night, you learn which character/role died by execution the previous day.
  *)
  | Monk
  (** each night, choose a player (not yourself): they are safe from the demon tonight.
  *)
  | Ravenkeeper
  (** if you die at night, you are worken to choose a player: you learn their character.
  *)
  | Virgin
  (** the 1st time you are nominated, if the nominator is a townsfolk, they are executed immediately.
  *)
  | Slayer
  (** once per game, during the day, publically choose a player: if they are the demon, they die.
  *)
  | Soldier (** you are safe from the demon. *)
  | Mayor
  (** if only 3 players live & no execution occurs, your team wins. if you die at night, another player might die instead.
  *)
  (* outsiders *)
  | Butler
  (** each night, choose a player (not yourself): tomorrow, you may only vote if they are voting too.
  *)
  | Drunk
  (** you do not know you are the drunk. you thikm you are a townsfolk character, but you are not.
  *)
  | Recluse
  (** you might register as evil & as a minion or demon, even if dead. *)
  | Saint (** if you die by execution, your team looses. *)
  (* minions *)
  | Poisoner
  (** each night, choose a player: they are poisoned tonight and tomorrow day. (this {i secretly} removes/negates their ability.)
  *)
  | Spy
  (** each night, you see the grimoire. you might register as good & as a townsfolk or outsider, even if dead. (i.e., the grimoire shows the state-of-the-game.)
  *)
  | ScarletWoman
  (** if there are 5 or more players alive & the demon dies, you become the demon. (Travellers don't count.)
  *)
  | Baron (** there are extra outsiders in play (+2). *)
  (* demons *)
  | Imp
  (** each night, choose a player: they die. if you kill yourself this way, a minion becomes the imp.
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

(** {2 Role alignment }*)

exception CannotDetermineAlignment of t

val alignment : t -> alignment
val allied : t -> t -> bool
val opposed : t -> t -> bool
