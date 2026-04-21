(** @canonical Game.Meta.Rounds *)

module Phase = Phase
module Round = Round

(** {1 Rounds} *)

(** linked-list of rounds *)
type t =
  | Initial of initial (** Initial game-state configuration. *)
  | Round of round (** Some round. *)

and round =
  { this : Round.t
  ; prev : t
  }

and initial =
  { phase : Phase.t (** Starting phase. *)
  ; players : Players.t (** Initial players. *)
  ; rolemap : bool Roles.Map.t
    (** Role selection map. {i (Required for implementing the {{!Role.t.Baron}Barons} setup ability.)}
    *)
  }

let make (phase : Phase.t) (rolemap : bool Roles.Map.t) (players : Players.t)
  : t
  =
  Initial { phase; players; rolemap }
;;

let initialize (x : initial) : t =
  Round { this = Round.make x.players; prev = Initial x }
;;

let players : t -> Players.t = function
  | Initial { players; _ } -> players
  | Round { this; _ } -> this.players
;;

(** {2 Traversal} *)

let rec initial : t -> initial = function
  | Initial x -> x
  | Round { prev; _ } -> initial prev
;;

let initial_phase (x : t) : Phase.t = (initial x).phase
let initial_players (x : t) : Players.t = (initial x).players
let rolemap (x : t) : bool Roles.Map.t = (initial x).rolemap

(** {2 Navigation} *)

exception SetupRound

let this : t -> Round.t = function
  | Initial _ -> raise SetupRound
  | Round { this; _ } -> this
;;

exception FirstRound

(** [prev x] returns {!field:prev} of [x].
    @raise SetupRound if [x] is [Initial _].
    @raise FirstRound if [x.prev] is [Initial _]. *)
let prev : t -> t = function
  | Initial _ -> raise SetupRound
  | Round { prev = Initial _; _ } -> raise FirstRound
  | Round { prev; _ } -> prev
;;

(** [step x] is the progression to the next {{!Phase.t}phase} of [x], which ultimately depends on the {!field:initial.phase}.
*)
let step (x : t) : t = Round { this = Round.make (players x); prev = x }

(** {2 Query} *)

(** [is_initial x] is [true] if [x] is {{!t.Initial}Initial}, else [false]. *)
let is_initial : t -> bool = function Initial _ -> true | _ -> false

(** [count x] is the number of rounds there have been in [x]. (Where {{!t.Initial}Initial} is 0.)
*)
let rec count : t -> int = function
  | Initial _ -> 0
  | Round { prev; _ } -> count prev + 1
;;

(** [num x] is the number of times the number of rounds [x] modulo the enum {{!Phase.t}phases}.
*)
let num (x : t) : int = Int.div Phase.max (count x)

(** {1 Round Phase} *)

let phase_of_int (x : t) : int -> Phase.t =
  Phase.of_int ~starting:(initial_phase x)
;;

let phase (x : t) : Phase.t = count x |> phase_of_int x

(** {2 Phase Query} *)

(** [SetupPhase] is raised when an {{!t.Initial}Initial} round is unexpectedly found.
*)
exception SetupPhase

(** [is_first_phase x] is [true] if [x.prev] is [Initial], else [false].
    @raise SetupPhase if [x] is [Initial _]. *)
let is_first_phase : t -> bool = function
  | Initial _ -> raise SetupPhase
  | Round { prev = Initial _; _ } -> true
  | _ -> false
;;

(** ...
    @raise SetupPhase
      if [x] is [Initial _] (since this would be trivially [true]). *)
let is_phase_start : t -> bool = function
  | Initial _ -> raise SetupPhase
  | Round x -> Phase.equal (initial_phase x.prev) (phase (Round x))
;;

(** ...
    @raise SetupPhase
      if [x] is [Initial _] (since something with no start cannot end). *)
let is_phase_end : t -> bool = function
  | Initial _ -> raise SetupPhase
  | Round { prev; _ } -> is_phase_start prev
;;
