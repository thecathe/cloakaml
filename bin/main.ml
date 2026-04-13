let () = print_endline "Hello, World!"

(*********************************************)

(** {b test:} neighbours *)
let () =
  print_endline "\ntest: neighbours";
  let players = Game.players in
  let min = Game.Players.min_elt players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let neighbours = Game.Players.neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Neighbours.show neighbours);
  ()
;;

(** {b test:} filter alignment neighbours *)
let () =
  print_endline "\ntest: filter alignment neighbours";
  let players = Game.players in
  let min = Game.Players.min_elt players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let allies = Game.Players.allied_neighbours min players in
  let opposed = Game.Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Game.Neighbours.show opposed);
  ()
;;

(** {b test:} player setup 5 *)
let () =
  print_endline "\ntest: player setup 5";
  let players = Game.Setup.players 5 in
  Printf.printf "players %s\n" (Game.Players.show players);
  let min = Game.Players.min_elt players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let allies = Game.Players.allied_neighbours min players in
  let opposed = Game.Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Game.Neighbours.show opposed);
  ()
;;

(** {b test:} player setup 10 *)
let () =
  print_endline "\ntest: player setup 10";
  let players = Game.Setup.players 10 in
  Printf.printf "players %s\n" (Game.Players.show players);
  let min = Game.Players.min_elt players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let allies = Game.Players.allied_neighbours min players in
  let opposed = Game.Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Game.Neighbours.show opposed);
  ()
;;

(** {b test:} player setup 15 *)
let () = 
  print_endline "\ntest: player setup 15";
  let players = Game.Setup.players 15 in
  Printf.printf "players %s\n" (Game.Players.show players);
  let min = Game.Players.min_elt players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let allies = Game.Players.allied_neighbours min players in
  let opposed = Game.Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Game.Neighbours.show opposed);
  ()
;;
