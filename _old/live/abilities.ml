(** @canonical Game.Live.Abilities *)

(** {1 Live Abilities} *)

(** [map_player map x] is a wrapper for {!Build.Abilities.map_player}. *)
let map_player : Build.Abilities.t -> Player.t -> unit =
  Build.Abilities.map_player
;;

let map_players (players : Build.Players.t) : Build.Abilities.t =
  Build.Abilities.map_players players
;;

(** {2 Filtering} *)

let map_players_kind (d : Data.t) (x : Trigger.Kind.t) : Build.Abilities.t =
  let xs = Build.Abilities.Query.Players.have_kind x (Data.players d) in
  (* |>  *)
  (* Players.iter (map_player (Data.abilties d)) *)
  let map = Data.abilties d |> Build.Players.Map.copy in
  Build.Players.Map.filter_map_inplace
    (fun k v -> if Build.Players.mem k xs then Some v else None)
    map;
  map
;;

let kind (d : Data.t) (x : Trigger.Kind.t) : Build.Abilities.t =
  map_players_kind d x
;;
