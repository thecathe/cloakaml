(** @canonical Game.Players.Player.Roles.Role *)

(** {1 Role} *)

(** ... *)
type t =
  (* townsfolk *)
  | Washerwoman
  (** You start knowing that 1 of 2 players is a particular {{!Kind.Townsfolk}Townsfolk}.
  *)
  | Librarian
  (** You start knowing that 1 of 2 players is a particular {{!Kind.Outsider}Outsider} (or that zero are in play).
  *)
  | Investigator
  (** You start knowing that 1 of 2 players is a particular {{!Kind.Minion}Minion}.
  *)
  | Chef
  (** You start knowing how many pairs of {{!Alignment.Evil}Evil} players there are.
  *)
  | Empath
  (** Each {{!Phase.Night}Night}, learn how many of your two {{!Players.Neighbours.t}Neighbours} are {{!Alignment.Evil}Evil}.
  *)
  | FortuneTeller
  (** Each {{!Phase.Night}Night}, choose 2 players: you learn if either is a {{!Kind.Demon}Demon}. There is a {{!Alignment.Good}Good} player that registers as a {{!Kind.Demon}Demon} to you (i.e., {b red herring}).
  *)
  | Undertaker
  (** Each {{!Phase.Night}Night}, you learn which {{!Role.t}Role} died by execution the previous {{!Phase.Day}Day}.
  *)
  | Monk
  (** Each {{!Phase.Night}Night}, choose a player (not yourself): they are {b safe} from the {{!Kind.Demon}Demon} tonight.
  *)
  | Ravenkeeper
  (** If you {b die} at {{!Phase.Night}Night}, you are woken to choose a player: you learn their {{!Role.t}Role}.
  *)
  | Virgin
  (** The first time you are {i nominated}, if the {b nominator} is a {{!Kind.Townsfolk}Townsfolk}, they are {i executed} immediately.
  *)
  | Slayer
  (** Once per game, during the {{!Phase.Day}Day}, publically choose a player: if they are the {{!Kind.Demon}Demon}, they {b die}.
  *)
  | Soldier (** You are {b safe} from the {{!Kind.Demon}Demon}. *)
  | Mayor
  (** If only 3 players live & no execution occurs, your team {b wins}. If you {b die} at {{!Phase.Night}Night}, another player {i might} {b die} instead.
  *)
  (* outsiders *)
  | Butler
  (** Each {{!Phase.Night}Night}, choose a player (not yourself): {i tomorrow}, you may only vote if they are voting too.
  *)
  | Drunk
  (** You do not know you are the drunk, i.e., you think you are a {{!Kind.Townsfolk}Townsfolk} character, but you are not.
  *)
  | Recluse
  (** You might register as {{!Alignment.Evil}Evil} and as a {{!Kind.Minion}Minion} or {{!Kind.Demon}Demon}, even if {b dead}.
  *)
  | Saint (** If you {b die} by {i execution}, your team {b loses}. *)
  (* minions *)
  | Poisoner
  (** Each {{!Phase.Night}Night}, choose a player: they are {i poisoned} tonight and tomorrow-day. (This {i secretly} removes/negates their ability.)
  *)
  | Spy
  (** Each {{!Phase.Night}Night}, you see the {b grimoire}. You might register as {{!Alignment.Good}Good} and as a {{!Kind.Townsfolk}Townsfolk} or {{!Kind.Outsider}Outsider}, even if {b dead}. {i (The {b grimoire} shows the state-of-the-game.)}
  *)
  | ScarletWoman
  (** If there are 5 or more players {b alive} and the {{!Kind.Demon}Demon} dies, you become the {{!Kind.Demon}Demon}. ({{!Kind.Traveller}Traveller}s don't count.)
  *)
  | Baron (** There are extra {{!Kind.Outsider}Outsider}s in play (+2). *)
  (* demons *)
  | Imp
  (** Each {{!Phase.Night}Night}, choose a player: they {b die}. If you kill yourself this way, a {{!Kind.Minion}Minion} becomes the {!Imp}.
  *)
[@@deriving show { with_path = false }, enum, eq]

exception EnumOutOfBounds

(** {2 Funs: Hash, Compare, Equal} *)

let hash (x : t) : int = Int.hash (to_enum x)
let compare (a : t) (b : t) : int = Int.compare (to_enum a) (to_enum b)
let equal = equal

(** {1 Kind} *)

module Kind = Kind

(** {2 Convert: Kind of Role} *)

let kind : t -> Kind.t = function
  (* townsfolk *)
  | Washerwoman -> Townsfolk
  | Librarian -> Townsfolk
  | Investigator -> Townsfolk
  | Chef -> Townsfolk
  | Empath -> Townsfolk
  | FortuneTeller -> Townsfolk
  | Undertaker -> Townsfolk
  | Monk -> Townsfolk
  | Ravenkeeper -> Townsfolk
  | Virgin -> Townsfolk
  | Slayer -> Townsfolk
  | Soldier -> Townsfolk
  | Mayor -> Townsfolk
  (* outsiders *)
  | Butler -> Outsider
  | Drunk -> Outsider
  | Recluse -> Outsider
  | Saint -> Outsider
  (* minions *)
  | Poisoner -> Minion
  | Spy -> Minion
  | ScarletWoman -> Minion
  | Baron -> Minion
  (* demons *)
  | Imp -> Demon
;;

(** {2 Funs: Roles is Kind} *)

let is_townfolk (x : t) : bool =
  match kind x with Townsfolk -> true | _ -> false
;;

let is_outsider (x : t) : bool =
  match kind x with Outsider -> true | _ -> false
;;

let is_minion (x : t) : bool = match kind x with Minion -> true | _ -> false
let is_demon (x : t) : bool = match kind x with Demon -> true | _ -> false

(** {1 Alignment} *)

module Alignment = Alignment

(** {2 Funs} *)

let is_good (x : t) : bool = is_townfolk x || is_outsider x
let is_evil (x : t) : bool = is_minion x || is_demon x

exception CannotDetermineAlignment of t

let alignment (x : t) : Alignment.t =
  if is_good x
  then Good
  else if is_evil x
  then Evil
  else raise (CannotDetermineAlignment x)
;;

let allied (a : t) (b : t) : bool =
  match alignment a, alignment b with
  | Good, Good -> true
  | Evil, Evil -> true
  | _, _ -> false
;;

let opposed (a : t) (b : t) : bool = allied a b |> Bool.not
