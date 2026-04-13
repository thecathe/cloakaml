module Roles = Roles
module Player = Player
module Neighbours = Neighbours
module Players = Players
module Round = Round
module Rounds = Rounds
module Abilities = Abilities

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

    (** see {{:https://i.redd.it/cunojgi5omnd1.jpeg}here} for the distributions for {b Blood on the Clocktower}. *)
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
      | n -> if n < 5 then raise (TooFewPlayers n) else make 9 2 3 1
    ;;

    let sum (x : t) : int = x.townsfolk + x.outsiders + x.minions + x.demons

    type role_status_map = (Roles.t, bool) Hashtbl.t

    let show_role_status_map (x : role_status_map) : string =
      String.cat
        (Hashtbl.fold
           (fun k v acc -> Printf.sprintf "%s%b: %s\n" acc v (Roles.show k))
           x
           "[\n")
        "]"
    ;;

    let fresh_role_status_map () : role_status_map =
      let tbl : (Roles.t, bool) Hashtbl.t = Hashtbl.create Roles.max in
      List.iter (fun x -> Hashtbl.add tbl x false) Roles.roles;
      tbl
    ;;

    type pull_role =
      { role : Roles.kind
      ; map : role_status_map
      }

    let available_roles (x : pull_role) : Roles.t list =
      Hashtbl.fold
        (fun k v acc ->
          if Roles.equal_kind x.role (Roles.kind k) && Bool.not v
          then k :: acc
          else acc)
        x.map
        []
    ;;

    let show_available_roles (x : Roles.kind) (xs : Roles.t list) : string =
      Printf.sprintf
        "available %s:%s\n"
        (Roles.show_kind x)
        (List.fold_left
           (fun acc x -> Printf.sprintf "%s %s" acc (Roles.show x))
           ""
           xs)
    ;;

    let pull_role (x : pull_role) : Roles.t =
      let xs = available_roles x in
      let y = List.nth xs (Random.int (List.length xs)) in
      Hashtbl.replace x.map y true;
      y
    ;;

    let rec fill_role (x : pull_role) (n : int) (acc : Players.t) : Players.t =
      if n <= 0
      then acc
      else Players.add_role (pull_role x) acc |> fill_role x (n - 1)
    ;;

    let fill_roles (x : t) : Players.t =
      Printf.printf "dist: %s\n" (show x);
      let map : role_status_map = fresh_role_status_map () in
      Players.empty
      |> fill_role { map; role = Townsfolk } x.townsfolk
      |> fill_role { map; role = Outsider } x.outsiders
      |> fill_role { map; role = Minion } x.minions
      |> fill_role { map; role = Demon } x.demons
    ;;

    let players (n : int) : Players.t = get n |> fill_roles
  end

  let players : int -> Players.t = Distribution.players
end

let round : Round.t = Round.initial players

(** create fresh round-env wrapper around active round *)
module RoundEnv = Live_round.Make (struct
    type data = Round.t

    let initial = round
  end)

type t = { round : Rounds.t }
