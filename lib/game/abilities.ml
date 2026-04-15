let fresh
      ?(round : Round.t option)
      ?(players : Players.t option = Option.map Round.players round)
      ?(n : int = Option.value (Option.map Players.cardinal players) ~default:0)
      ()
  : Ability.t
  =
  Player.Map.create n
;;

let player_map (map : Ability.t) (x : Player.t) : Ability.t =
  Ability.map_player ~map x
;;

let map_player (map : Ability.t) (x : Player.t) : unit =
  let _ = player_map map x in
  ()
;;

let map_players (round : Round.t) ?(map = fresh ~round ()) (x : Ability.kind)
  : Ability.t
  =
  Ability.players_have_kind x round.players |> Players.iter (map_player map);
  map
;;

let get (round : Round.t) ?(map = fresh ~round ()) (x : Ability.kind)
  : Ability.t
  =
  map_players round ~map x
;;
