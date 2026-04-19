module Ability = Ability
module Query = Query
module AbilityMap = Ability_map

(** {1 Map} *)

type t = AbilityMap.t' Players.Map.t

let fresh
      ?(round : Round.t option)
      ?(players : Players.t option = Option.map Round.players round)
      ?(n : int = Option.value (Option.map Players.cardinal players) ~default:0)
      ()
  : t
  =
  Players.Map.create n
;;

let map_player (map : t) (x : Player.t) : t =
  Players.Map.replace map x (AbilityMap.make x);
  map
;;

let map_players (xs : Players.t) : t =
  Players.cardinal xs
  |> Players.Map.create
  |> Players.fold (fun x map -> map_player map x) xs
;;

let kind_opt (x : Trigger.Kind.t)
  : Player.t -> AbilityMap.t' -> AbilityMap.t' option
  =
  fun k v ->
  let map = AbilityMap.copy v in
  AbilityMap.reduce_by_kind map x;
  if AbilityMap.length map <= 0 then None else Some map
;;

let of_kind (y : Trigger.Kind.t) (x : Round.t) : t =
  let map = map_players x.players in
  Players.Map.filter_map_inplace (kind_opt y) map;
  map
;;

let players (map : t) = Players.Map.to_seq_keys map |> Players.of_seq
