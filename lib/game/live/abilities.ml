(** @canonical Game.Live.Abilities *)

(** {1 Live Abilities} *)

(** [map_player map x] is a wrapper for {!Meta.Abilities.map_player}. *)
let map_player : Meta.Abilities.t -> Player.t -> unit =
  Meta.Abilities.map_player
;;

let map_players (players : Meta.Players.t) : Meta.Abilities.t =
  Meta.Abilities.map_players players
;;

(** {2 Filtering} *)

let map_players_kind (d : Data.t) (x : Trigger.Kind.t) : Meta.Abilities.t =
  let xs = Meta.Abilities.Query.Players.have_kind x (Data.players d) in
  (* |>  *)
  (* Players.iter (map_player (Data.abilties d)) *)
  let map = Data.abilties d |> Meta.Players.Map.copy in
  Meta.Players.Map.filter_map_inplace
    (fun k v -> if Meta.Players.mem k xs then Some v else None)
    map;
  map
;;

let kind (d : Data.t) (x : Trigger.Kind.t) : Meta.Abilities.t =
  map_players_kind d x
;;
