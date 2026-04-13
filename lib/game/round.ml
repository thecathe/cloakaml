module Phase = struct
  type t =
    | Day
    | Night
  [@@deriving show { with_path = false }, eq]

  let dual : t -> t = function Day -> Night | Night -> Day

  type data =
    { starting : t
    ; mutable current : t
    }
  [@@deriving show { with_path = false }, make]

  let make (starting : t) : data = make_data ~starting ~current:starting

  (* *)
  let is_start_phase (x : data) : bool = equal x.starting x.current
  let is_end_phase (x : data) : bool = Bool.not (is_start_phase x)

  exception StartNewRound

  let step (x : data) : data =
    if equal x.starting x.current
    then (
      x.current <- dual x.current;
      x)
    else raise StartNewRound
  ;;
end

(** tracks turn counter and players *)
type t =
  { num : int
  ; players : Players.t
  ; phase : Phase.data
  }

let initial ?(starting : Phase.t = Phase.Day) (players : Players.t) : t =
  { num = 0; players; phase = Phase.make starting }
;;

let is_phase (x : t) : Phase.t -> bool = Phase.equal x.phase.current

(** [is_tonight n x] is [true] if [x.phase.current] is {{!Phase.Night}Night} and [x.num] is equal to [n], i.e., [n] is the turn number collected in the {{!Phase.Day}Day} for an ability to be used at {{!Phase.Night}Night}.
*)
let is_tonight (n : int) (x : t) : bool = Int.equal n x.num && is_phase x Night

(** [is_today n x] is {i dual} respective to {!is_tonight} but for {{!Phase.Day}Day}.
*)
let is_today (n : int) (x : t) : bool = Int.equal n x.num && is_phase x Day

(** [is_tomorrow n x] returns [is_today (n + 1) x]. *)
let is_tomorrow (n : int) (x : t) : bool = is_today (n + 1) x
