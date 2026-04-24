(** @canonical Cloakaml.Game *)

(** [module Mode] ... *)
module Mode = Mode

(** [module Build] ... *)
module Build = Build

(** [module Setup] is ... *)
module Setup = Setup

(** [module Live] is ... *)
module Live = Live

(** [module Server] is ... *)
module Server = Server

(** {1 todo} *)

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
