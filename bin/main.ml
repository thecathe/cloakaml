let () = print_endline "Hello, World!"

(*********************************************)

(** {b test:} neighbours *)
let () =
  print_endline "\ntest: neighbours";
  let players = Game.Make.players in
  let min = Game.Players.min_elt players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let neighbours = Game.Players.neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Players.Neighbours.show neighbours);
  ()
;;

(** {b test:} filter alignment neighbours *)
let () =
  print_endline "\ntest: filter alignment neighbours";
  let players = Game.Make.players in
  let min = Game.Players.min_elt players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let allies = Game.Players.allied_neighbours min players in
  let opposed = Game.Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Players.Neighbours.show allies);
  Printf.printf "opposed neighbours %s\n" (Game.Players.Neighbours.show opposed);
  ()
;;
