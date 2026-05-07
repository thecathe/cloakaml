(** @canonical Cloakaml.Modes.BloodOnTheClockTower *)

module TheRoles = The_roles
module ThePlayers = The_players
module TheRounds = The_rounds
module TheTriggers = The_triggers

(** {1 Mode: Blood on the Clocktower} *)

module Build () = struct
  module Roles = TheRoles.Build ()
  module Players = ThePlayers.Build ()
  module Rounds = TheRounds.Build ()
  (* module Triggers = TheTriggers.Build () *)
end

(*******************)

(** {1 Sketch of Abilities} *)

include Build ()

type phase = Rounds.Round.Data.phase
type status = Players.Player.status

(* type ability = { trigger : trigger; condition: } *)
type ability = { is_enabled : enabled_when }

and enabled_when =
  | Always (** Always enabled *)
  | If of condition (** If condition holds *)
  | Triggered of event (** Triggered by event *)

and condition =
  | Query of query (** Query the state *)
  | On of event (** At the beginning of the event *)
  | After of event (** At the end of the event *)
  | Is of phase (** Is the phase *)
  | During of scope * event * query (** If query holds in specific event *)
  | And of condition * condition (** Both conditions *)
  | Or of condition * condition
  | Not of condition
  | All of condition list

and event =
  | Phase of phase
  | Status of status
  | Status_At of status * event

and scope = Previous

and query =
  | Any of history
  | Num of (countable * query_op * int)
  | And of (query * query)
  | Not of query

and history =
  | Death
  | Execution
  | Nomination

and countable =
  | Players
  | Group of Roles.group

and query_op =
  | Eq
  | Lt
  | Leq
  | Gt
  | Geq

(** {2 Infixes} *)

let death = Status Dead
let on_death = On death

(* *)
let nomination = Status Nominated
let on_nomination = On nomination

(* *)
let night = Phase Night
let each_night = On night
let last_night x = During (Previous, night, x)

(* *)
let day = Phase Day
let each_day = On day
let yesterday x = During (Previous, day, x)

(* *)
let there_is x = Query x
let there_are = there_is
let num c op n = Num (c, op, n)
let ( == ) c n = c Eq n

(** {2 Examples} *)

(** {3 Mayor} *)

(** {4 Part 1: If only 3 players live & no execution occurs, your team {b wins}.}
*)

let end_of_day : condition = After day
let only_3_players_alive : query = num Players == 3
let no_execution_occurred : condition = yesterday (Not (Any Nomination))

let mayor_1 : enabled_when =
  If (All [ end_of_day
          ; there_are only_3_players_alive
          ; no_execution_occurred
          ])
;;

(** {4 Part 2: If you {b die} at night, another player {i might} {b die} instead.}
*)

let mayor_2 = Triggered (Status_At (Dead, night))
