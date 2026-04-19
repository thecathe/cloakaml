module Round = Round

(** linked-list into previous rounds *)
type t =
  { this : Round.t
  ; prev : t option
  }

exception FirstRound

let prev : t -> t = function
  | { prev = None; _ } -> raise FirstRound
  | { prev = Some x; _ } -> x
;;

exception FirstPhase

(** [back x] the last phase before this current one, raising {!FirstPhase} if this is the first phase of the first round.
*)
let back (x : t) : Phase.t =
  if Phase.is_start_phase x.this.phase && Option.is_none x.prev
  then raise FirstPhase
  else Phase.dual x.this.phase.current
;;
