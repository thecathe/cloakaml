(** [module Players] is ... *)
module Players = Players

(** [module Rounds] is ... *)
module Rounds = Rounds

(** [module Abilities] is ... *)
module Abilities = Abilities

(** [module Live] is ... *)
module Live = Live

type t = { round : Rounds.t }

module Debug = struct
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

  (* let round : Round.t = Setup.round Day 5

     (** create fresh round-env wrapper around active round *)
     module RoundEnv = Live_round.Make (struct
     type data = Round.t

     let initial = round
     end) *)
end
