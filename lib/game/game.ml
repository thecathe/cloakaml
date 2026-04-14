module Roles = Roles
module Phase = Phase
module Abilities = Abilities
module Player = Player
module Neighbours = Neighbours
module Players = Players
module Round = Round
module Rounds = Rounds
module Setup = Setup

let players : Players.t =
  Players.create
    [ Player.create 0 Washerwoman
    ; Player.create 1 Librarian
    ; Player.create 2 Investigator
    ; Player.create 3 Chef (*********)
    ; Player.create 4 Poisoner
    ; Player.create 5 Imp
    ]
;;

let round : Round.t = Round.initial players

(** create fresh round-env wrapper around active round *)
module RoundEnv = Live_round.Make (struct
    type data = Round.t

    let initial = round
  end)

type t = { round : Rounds.t }
