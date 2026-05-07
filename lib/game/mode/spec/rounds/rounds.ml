(** @canonical Game.Mode.Spec.Rounds *)

type ('round, 'initial) t =
  { round : 'round option
  ; initial : 'initial
  }

module type S = sig
  module Round : Round.S

  type round = Round.t
  type data = Round.data
  type initial = Round.Data.initial
  type players = Round.players
  type nonrec t = (round, initial) t

  val create : initial -> players -> t

  exception InitializingRounds

  val round : t -> round
  val round_opt : t -> round option
  val is_initial : t -> bool
  val initial : t -> initial
  val current : t -> data

  exception UninitializedRound

  val next : t -> t
  val prev : t -> t
  val count : t -> int
end

module Make (R : Round.S) : S with module Round = R = struct
  module Round = R

  type round = Round.t
  type data = Round.data
  type initial = Round.Data.initial
  type players = Round.players
  type nonrec t = (round, initial) t

  let create (initial : initial) (players : players) : t =
    { round = Some (Round.create initial players); initial }
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

  exception UninitializedRound

  let next (x : t) : t =
    let round : round option =
      try Some (round x |> Round.next) with
      | InitializingRounds -> raise UninitializedRound
    in
    { x with round }
  ;;

  let prev (x : t) : t = { x with round = Some (round x |> Round.prev) }

  let count (x : t) : int =
    try round x |> Round.index with InitializingRounds -> 0
  ;;
end
