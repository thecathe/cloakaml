let () = print_endline "Hello, World!"
let log : string -> unit = Printf.printf "%s\n"
let logl (x : string) : unit = Printf.sprintf "\n%s" x |> log

open Game

module Utils = struct
  let setup_players (n : int) : Players.t =
    let players = Setup.players n in
    Printf.printf "players (%i) %s\n" n (Players.show players);
    players
  ;;

  let setup_round (n : int) : Round.t = Setup.round Day n

  let get_neighbours
        ?(f : Player.t -> Players.t -> Players.t = fun x -> fun ys -> ys)
        (xs : Players.t)
        ?(player = Players.random xs)
        ()
    : Neighbours.t
    =
    Printf.sprintf "neighbours (of %s)" (Player.show player) |> log;
    let neighbours = Players.neighbours ~f:(f player) player xs in
    Printf.sprintf "neighbours: %s" (Neighbours.show neighbours) |> log;
    neighbours
  ;;
end

module Tests = struct
  open Utils

  let players_setup (n : int) =
    let _ = setup_players n in
    ()
  ;;

  let neighbours (xs : Players.t) ?(player = Players.random xs) () : unit =
    try
      let _ = get_neighbours xs ~player () in
      ()
    with
    | Neighbours.NoNeighboursFound ->
      Printf.sprintf "Err: no neighbours found: %s" (Players.show xs) |> log
  ;;

  let player_neighbours (n : int) ?(players = setup_players n) () : unit =
    neighbours players ()
  ;;

  let neighbours_filter
        (xs : Players.t)
        ?(player = Players.random xs)
        (f : Players.t -> Players.t)
    : unit
    =
    try
      let _ =
        get_neighbours ~f:(Players.incl_self (fun _ -> f)) xs ~player ()
      in
      ()
    with
    | Neighbours.NoNeighboursFound ->
      Printf.sprintf "Err: no neighbours found: %s" (Players.show xs) |> log
  ;;

  let player_neighbours_group player players (x : Roles.group) : unit =
    Printf.sprintf "neighbours (group: %s)" (Roles.show_group x) |> log;
    neighbours_filter players ~player (Players.group x)
  ;;

  let player_neighbours_group_kind player players : unit =
    logl "neighbours: kinds";
    player_neighbours_group player players (Roles.Kind Townsfolk);
    player_neighbours_group player players (Roles.Kind Outsider);
    player_neighbours_group player players (Roles.Kind Minion);
    player_neighbours_group player players (Roles.Kind Demon)
  ;;

  let player_neighbours_group_alignment player players : unit =
    logl "neighbours: alignment";
    player_neighbours_group player players (Roles.Alignment Good);
    player_neighbours_group player players (Roles.Alignment Evil)
  ;;

  let player_neighbours_groups (n : int) ?(players = setup_players n) () : unit =
    let player = Players.random players in
    player_neighbours_group_kind player players;
    player_neighbours_group_alignment player players
  ;;

  let player_neighbours_status player players (x : Player.Status.t) : unit =
    Printf.sprintf "neighbours (status: %s)" (Player.Status.show x) |> logl;
    neighbours_filter players ~player (Players.status x)
  ;;

  let player_neighbours_statuses (n : int) ?(players = setup_players n) ()
    : unit
    =
    let player = Players.random players in
    player_neighbours_status player players Player.Status.Alive;
    player_neighbours_status player players Player.Status.Dead;
    (* player_neighbours_status player players Player.Status.Poisoned; *)
    ()
  ;;
end

(*********************************************)

module Run = struct
  open Tests

  let log (x : string) (n : int) : unit =
    Printf.sprintf "test (n: %i) : %s " n x |> logl
  ;;

  let players_setup (n : int) : unit =
    log "setup players" n;
    players_setup n
  ;;

  let player_neighbours (n : int) : unit =
    log "neighbours" n;
    player_neighbours n ()
  ;;

  let player_neighbours_groups (n : int) : unit =
    log "neighbours (filter groups)" n;
    player_neighbours_groups n ()
  ;;

  let player_neighbours_statuses (n : int) : unit =
    log "neighbours (filter statuses)" n;
    player_neighbours_statuses n ()
  ;;
end

(** {1 Tests} *)

(** [n] is the number of players to use in the tests. *)
let n : int = 5

(** {2 Player Setup} *)

(** {b Test:} show randomally generated set of [n] players. *)
let () = Run.players_setup n

(** {2 Neighbours} *)

(** {b Test:} show neighbours of random player out of set of [n] players. *)
let () = Run.player_neighbours n

(** {3 Filter Neighbours by Role Groups} *)

(** {b Test:} show neighbours by groups. *)
let () = Run.player_neighbours_groups n

(** {b Test:} show neighbours by statuses. *)
let () = Run.player_neighbours_statuses n

(** {2 Round} *)

(** {3 Progression} *)

(** {b Test:} *)
let () = ()

(** {3 Abilities} *)

(** {3 s} *)

let round_active_abilities (round : Round.t) : unit =
  Printf.printf "players %s\n" (Players.show round.players);
  let xs = Round.players_with_active_abilities round in
  Printf.printf "active %s\n" (Players.show xs);
  let ys = Round.players_with_phase_abilities round in
  Printf.printf "phase %s\n" (Players.show ys);
  ()
;;

(** {b test:} 5 players, first day, who has active abilities *)
let _s () =
  print_endline "\ntest: 5 players, first day, who has active abilities";
  let round = Setup.round Day 5 in
  round_active_abilities round
;;

(** {b test:} 5 players, first night, who has active abilities *)
let () =
  print_endline "\ntest: 5 players, first night, who has active abilities";
  let round = Setup.round Night 5 in
  round_active_abilities round
;;
