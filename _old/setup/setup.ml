(** @canonical Game.Setup *)

module Distribution = Distribution

let players ?(map : bool Roles.Map.t = Distribution.fresh_role_status_map ())
  : int -> Players.t
  =
  Distribution.players map
;;

(** [rounds starting n] is the {{!Rounds.t.Initial}initial} round, with [starting] phase and [n] players.
*)
let rounds (x : Phase.t) (n : int) : Rounds.t =
  let map : bool Roles.Map.t = Distribution.fresh_role_status_map () in
  players ~map n |> Rounds.make x map
;;
