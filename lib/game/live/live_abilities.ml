open Abilities

let player_map (map : t) (x : Player.t) : t = map_player map x

let map_player (map : t) (x : Player.t) : unit =
  let _ = player_map map x in
  ()
;;

let map_players (round : Round.t) ?(map = fresh ~round ()) (x : Trigger.Kind.t)
  : t
  =
  Query.Players.have_kind x round.players |> Players.iter (map_player map);
  map
;;

let get (round : Round.t) ?(map = fresh ~round ()) (x : Trigger.Kind.t) : t =
  map_players round ~map x
;;
