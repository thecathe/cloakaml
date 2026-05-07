(** @canonical Game.Mode.Spec.Round *)

module Data = Data

type 'data t =
  { this : 'data
  ; prev : 'data t option
  }

module type S = sig
  module Data : Data.S

  type phase = Data.phase
  type data = Data.t
  type initial = Data.initial
  type player = Data.Players.player
  type players = Data.players
  type group = Data.Players.Player.group
  type nonrec t = data t

  val create : initial -> players -> t
  val data : t -> data
  val next : t -> t
  val prev : t -> t
  val index : t -> int
  val phase : t -> phase
  val players : t -> players
  val show : t -> string
  val is_phase : phase -> t -> bool
  val random_player : ?f:(players -> players) -> t -> player
end

module Make (D : Data.S) : S with module Data = D and type t = D.t t = struct
  module Data = D

  type phase = Data.phase
  type data = Data.t
  type initial = Data.initial
  type player = Data.Players.player
  type players = Data.players
  type group = Data.Players.Player.group
  type nonrec t = data t

  let create (initial : Data.initial) (players : players) : t =
    { this = D.create initial players; prev = None }
  ;;

  let data (x : t) : data = x.this
  let next (x : t) : t = { this = data x |> D.next; prev = Some x }

  exception NoPrevRound

  let prev : t -> t = function
    | { prev = None; _ } -> raise NoPrevRound
    | { prev = Some x; _ } -> x
  ;;

  let rec index : t -> int = function
    | { prev = None; _ } -> 1
    | { prev = Some x; _ } -> 1 + index x
  ;;

  let phase (x : t) : phase = index x |> D.Phase.of_int
  let players (x : t) : players = x.this.players
  let show (x : t) : string = "TODO: show"
  let is_phase (a : phase) (x : t) : bool = phase x |> D.Phase.equal a

  let random_player ?(f : (Data.Players.t -> Data.Players.t) option) (x : t)
    : player
    =
    match f with
    | None -> Data.Players.random x.this.players
    | Some f -> Data.Players.random ~f x.this.players
  ;;
end
