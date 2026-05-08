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

module Event = struct
  type 'a transition =
    { before : 'a
    ; after : 'a
    }

  let transition (before : 'a) (after : 'a) : 'a transition = { before; after }

  type phase =
    | Day
    | Night

  type status =
    | Alive
    | Dead
    | Alive_And of extra
  [@@deriving show { with_path = false }, eq]

  and extra =
    | Asleep
    | Nominated
    | Poisoned
    | Both of (extra * extra)

  type change =
    | Phase of phase transition
    | Status of status transition

  (* *)
  let phase_transition : phase -> phase -> phase transition = transition

  (* *)
  let day_to_night : phase transition = phase_transition Day Night
  let night_to_day : phase transition = phase_transition Night Day

  (* *)
  let phase_change (x : phase transition) : change = Phase x
  let end_of_day : change = phase_change day_to_night
  let end_of_night : change = phase_change night_to_day

  (* *)
  let start_of_day : change = end_of_night
  let start_of_night : change = end_of_day

  (* *)
  let status_transition : status -> status -> status transition = transition

  (* *)
  let died : status transition = status_transition Alive Dead
  let is_nominated : status = Alive_And Nominated
  let nominated : status transition = status_transition Alive is_nominated

  (* *)
  let status_change (x : status transition) : change = Status x
  let death : change = status_change died
  let nomination : change = status_change nominated

  (** ew ugly -- this is what we would have to do without the above *)
  let nomination : change =
    Status { before = Alive; after = Alive_And Nominated }
  ;;
end

(* type ability = { trigger : trigger; condition: } *)
type ability = { is_enabled : enabled_when }

and enabled_when =
  | Always (** Always enabled *)
  | If of condition (** If condition holds *)
  | Iff of event_sequence (** If-and-only-if sequence of events holds *)
  | Triggered of event (** Triggered by event *)

and event_sequence =
  | First of event_sequence
  | Next of event * event_sequence
  | Finally of event

and condition =
  | Query of query (** Query the state *)
  | During of scope * event * query (** If query holds in specific event *)
  | And of condition * condition (** Both conditions *)
  | Or of condition * condition
  | Not of condition
  | All of condition list

and event =
  | On of event (** At the beginning of the event *)
  | After of event (** At the end of the event *)
  | Is of phase (** Is the phase *)
  | Status_At of status * event

and event_change =
  | Phase of phase event_transition
  | Status of status event_transition

and 'a event_transition =
  { before : 'a
  ; after : 'a
  }

and event_frame =
  | Phase of phase
  | Status_Change of status

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

(* let death = Status Dead
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
  If (All [ end_of_day; there_are only_3_players_alive; no_execution_occurred ])
;;

let mayor_1 : enabled_when =
  Iff
    (First
       (Next
          ( end_of_day
          , Next
              (no_execution_occurred, Finally (there_are only_3_players_alive))
          )))
;;

(** {4 Part 2: If you {b die} at night, another player {i might} {b die} instead.}
*)

let mayor_2 = Triggered (Status_At (Dead, night)) *)
