(** @canonical Game.Meta.Players.Player.Status *)

type t =
  | Alive
  | Dead
  | Poisoned
[@@deriving show { with_path = false }, eq]

let initial : t = Alive
let poisoned : t -> bool = function Poisoned -> true | _ -> false
let alive : t -> bool = function Alive -> true | x -> poisoned x
let dead : t -> bool = function Dead -> true | _ -> false
