let () = print_endline "Hello, World!"

(*********************************************)

(** {b test:} neighbours *)
let () =
  let players = Game.Make.players in
  let min = Game.Players.min players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let { left; right } : Game.Players.neighbours =
    Game.Players.neighbours min players
  in
  Printf.printf "left %s\n" (Game.Roles.show left.role);
  Printf.printf "right %s\n" (Game.Roles.show right.role);
  ()
;;

(** {b test:} filter alignment neighbours *)
let () =
  let players = Game.Make.players in
  let min = Game.Players.min players in
  Printf.printf "min %s\n" (Game.Roles.show min.role);
  let allies = Game.Players.allied_neighbours min players in
  let opposed = Game.Players.opposed_neighbours min players in
  Printf.printf "allied neighbours %s\n" (Game.Players.show_neighbours allies);
  Printf.printf "opposed neighbours %s\n" (Game.Players.show_neighbours opposed);
  () 
;;
