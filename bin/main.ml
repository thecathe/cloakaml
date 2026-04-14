let () = print_endline "Hello, World!"

(*********************************************)

open Game

(** {b test:} neighbours *)
let () =
  print_endline "\ntest: neighbours";
  let min = Players.min_elt players in
  Printf.printf "min %s\n" (Roles.show min.role);
  let neighbours = Players.neighbours min players in
  Printf.printf "allied neighbours %s\n" (Neighbours.show neighbours);
  ()
;;

(** {b test:} filter alignment neighbours *)
let () =
  print_endline "\ntest: filter alignment neighbours";
  let min = Players.min_elt players in
  Printf.printf "min %s\n" (Roles.show min.role);
  let allies = Players.allied_neighbours min players in
  let opposed = Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Neighbours.show opposed);
  ()
;;

(** {b test:} player setup 5 *)
let () =
  print_endline "\ntest: player setup 5";
  let players = Setup.players 5 in
  Printf.printf "players %s\n" (Players.show players);
  let min = Players.min_elt players in
  Printf.printf "min %s\n" (Roles.show min.role);
  let allies = Players.allied_neighbours min players in
  let opposed = Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Neighbours.show opposed);
  ()
;;

(** {b test:} player setup 10 *)
let () =
  print_endline "\ntest: player setup 10";
  let players = Setup.players 10 in
  Printf.printf "players %s\n" (Players.show players);
  let min = Players.min_elt players in
  Printf.printf "min %s\n" (Roles.show min.role);
  let allies = Players.allied_neighbours min players in
  let opposed = Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Neighbours.show opposed);
  ()
;;

(** {b test:} player setup 15 *)
let () =
  print_endline "\ntest: player setup 15";
  let players = Setup.players 15 in
  Printf.printf "players %s\n" (Players.show players);
  let min = Players.min_elt players in
  Printf.printf "min %s\n" (Roles.show min.role);
  let allies = Players.allied_neighbours min players in
  let opposed = Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Neighbours.show opposed);
  ()
;;

let round_active_abilities (round : Round.t) : unit =
  Printf.printf "players %s\n" (Players.show round.players);
  let xs = Round.players_with_active_abilities round in
  Printf.printf "active %s\n" (Players.show xs);
  let ys = Round.players_with_phase_abilities round in
  Printf.printf "phase %s\n" (Players.show ys);
  ()
;;

(** {b test:} 5 players, first day, who has active abilities *)
let () =
  print_endline "\ntest: 5 players, first day, who has active abilities";
  let round = Setup.round Day 5 in
  round_active_abilities round
;;

(** {b test:} 5 players, first night, who has active abilities *)
let _s () =
  print_endline "\ntest: 5 players, first night, who has active abilities";
  let round = Setup.round Night 5 in
  round_active_abilities round
;;
