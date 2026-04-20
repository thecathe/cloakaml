(** @canonical Game.Setup *)

module Distribution = Distribution

let players ?(map : bool Roles.Map.t = Distribution.fresh_role_status_map ())
  : int -> Players.t
  =
  Distribution.players map
;;

let round (starting : Phase.t) (n : int) : Round.t =
  let map : bool Roles.Map.t = Distribution.fresh_role_status_map () in
  players ~map n |> Round.initial ~starting map
;;
