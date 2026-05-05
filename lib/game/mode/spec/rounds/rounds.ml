(** @canonical Game.Mode.Spec.Rounds *)

module Phase = Phase
module Data = Data
module Round = Round
(* module Kind = Kind *)

type ('round, 'initial) t =
  { round : 'round option
  ; initial : 'initial
  }

module type S = sig
  type round
  type data
  type initial
  type players
  type nonrec t = (round, initial) t

  val create : players -> ?initial:initial -> ?round:round -> unit -> t

  exception InitializingRounds

  val round : t -> round
  val round_opt : t -> round option
  val is_initial : t -> bool
  val initial : t -> initial
  val current : t -> data
  val next : t -> t
  val prev : t -> t
  val count : t -> int
end

module type InputS = sig end

module Make
    (P : Phase.S)
    (D : Data.S with type phase = P.t)
    (R :
       Round.S
       with type phase = P.t
        and type data = D.t
        and type players = D.players)
    (X : InputS) :
  S
  with type round = R.t
   and type data = D.t
   and type initial = D.initial
   and type players = R.players = struct
  type round = R.t
  type data = D.t
  type initial = D.initial
  type players = R.players
  type nonrec t = (round, initial) t

  let create
        (players : players)
        ?(initial : initial = D.initial players ())
        ?(round : round = R.create ~phase:initial.starting_phase players)
        ()
    : t
    =
    { round = Some round; initial }
  ;;

  exception InitializingRounds

  let round : t -> round = function
    | { round = None; _ } -> raise InitializingRounds
    | { round = Some x; _ } -> x
  ;;

  let round_opt (x : t) : round option = x.round

  let is_initial : t -> bool = function
    | { round = None; _ } -> true
    | _ -> false
  ;;

  let initial (x : t) : initial = x.initial
  let current (x : t) : data = (round x).this

  let next (x : t) : t =
    let round : round option =
      try Some (round x |> R.next) with
      | InitializingRounds ->
        let { starting_phase; players; _ } : initial = initial x in
        Some (R.create ~phase:starting_phase players)
    in
    { x with round }
  ;;

  let prev (x : t) : t = { x with round = Some (round x |> R.prev) }

  let count (x : t) : int =
    try round x |> R.index with InitializingRounds -> 0
  ;;
end
