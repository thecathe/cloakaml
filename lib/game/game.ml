module Roles = Roles
module Player = Player
module Neighbours = Neighbours
module Players = Players
module Round = Round
module Rounds = Rounds

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

module Setup = struct
  exception TooFewPlayers of int

  module Distribution = struct

  type t =
    { mutable townsfolk : int
    ; mutable outsiders : int
    ; mutable minions : int
    ; mutable demons : int
    }
  [@@deriving show { with_path = false }, make]

  let make townsfolk outsiders minions demons =
    make ~townsfolk ~outsiders ~minions ~demons
  ;;


  (** see {{:https://i.redd.it/cunojgi5omnd1.jpeg}here}. *)
  let get : int -> t = function
    | 5 -> make 3 0 1 1
    | 6 -> make 3 1 1 1
    | 7 -> make 5 0 1 1
    | 8 -> make 5 1 1 1
    | 9 -> make 5 2 1 1
    | 10 -> make 7 0 2 1
    | 11 -> make 7 1 2 1
    | 12 -> make 7 2 2 1
    | 13 -> make 9 0 3 1
    | 14 -> make 9 1 3 1
    | n ->
      if n < 5
      then raise (TooFewPlayers n)
      else make 9 2 3 1
  ;;

  let sum (x:t) : int = x.townsfolk + x.outsiders + x.minions + x.demons

  exception NothingToPull

  let pull_kind (n:int) (x:t) : Roles.kind = 
    if x.townsfolk > 0 && n <= x.townsfolk then (
      x.townsfolk <- x.townsfolk - 1;
      Roles.Townsfolk
    ) else (
      if x.outsiders > 0 && n <= x.outsiders then (
      x.outsiders <- x.outsiders - 1;
      Roles.Outsider
    ) else (
      if x.minions > 0 && n <= x.minions then (
      x.minions <- x.minions - 1;
      Roles.Minion
    ) else (
      if x.demons > 0 && n <= x.demons then (
      x.demons <- x.demons - 1;
      Roles.Demon
    ) else (
      raise NothingToPull
    )
    )
    )
    )

  exception ErrorWhenInitialisingRoleStatuses

    let role_statuses : (Roles.t, bool) Hashtbl.t = 
      let tbl  : (Roles.t, bool) Hashtbl.t = Hashtbl.create Roles.max in
      let rec iter (n:int) : unit =
        if n < Roles.min then () else (
          Hashtbl.add tbl (
            
          match Roles.of_enum n with 
          | None -> raise ErrorWhenInitialisingRoleStatuses
          | Some x -> (x)
          ) false; iter (n - 1)
        )
      in
      iter Roles.max;
      tbl
    ;;

    let pull (n:int) (x:t) : Roles.t = 
      (* TODO: use [pull_kind] to determine the role to get with the new [Roles.townsfolk], and then pull one from there that isn't in [role_statuses] *)
      Roles.Washerwoman
    ;;
end


  let players (n : int) : Players.t =
    let dist = Distribution.get n in
    let xs : Players.t = Players.empty in
    Random.self_init ();
    let iter (n:int) (acc:Players.t) : Players.t = 
      if n <=0 then acc else 
        let role : Roles.t = (Distribution.pull (Random.int n) dist) in
    Players.add_role role xs
    in iter n
    xs
  ;;
end

let round : Round.t = Round.initial players

(** create fresh round-env wrapper around active round *)
module RoundEnv = Live_round.Make (struct
    type data = Round.t

    let initial = round
  end)

type t = { round : Rounds.t }
