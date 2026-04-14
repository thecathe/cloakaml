module Players : Set.S with type elt = Player.t = Set.Make (Player)
include Players

let show (xs : t) : string =
  String.cat
    (fold
       (fun x acc -> String.cat (Player.show x) "\n" |> String.cat acc)
       xs
       "[\n")
    "]"
;;

let add_role (x : Roles.t) (ys : t) : t = add (Player.create (cardinal ys) x) ys
let create (xs : Player.t list) : t = of_list xs

let random (xs : t) : Player.t =
  Random.self_init ();
  List.nth (to_list xs) (Random.int (cardinal xs))
;;

let alive : t -> t = filter Player.alive
let dead : t -> t = filter Player.dead
let allied (x : Player.t) : t -> t = filter (Player.allied x)
let opposed (x : Player.t) : t -> t = filter (Player.opposed x)

let neighbours (x : Player.t) (ys : t) : Neighbours.t =
  to_list ys |> Neighbours.find x
;;

let filter_neighbours (f : t -> t) (x : Player.t) (ys : t) : Neighbours.t =
  f ys |> add x |> neighbours x
;;

let allied_neighbours (x : Player.t) : t -> Neighbours.t =
  filter_neighbours (allied x) x
;;

let opposed_neighbours (x : Player.t) : t -> Neighbours.t =
  filter_neighbours (opposed x) x
;;

let alive_neighbours : Player.t -> t -> Neighbours.t = filter_neighbours alive
let dead_neighbours : Player.t -> t -> Neighbours.t = filter_neighbours dead

let with_active_abilities : t -> t =
  filter (fun y ->
    Abilities.make y.role |> Abilities.active |> List.is_empty |> Bool.not)
;;

let with_phase_abilities (x : Phase.t) : t -> t =
  filter (fun y ->
    Abilities.make y.role |> Abilities.phase x |> List.is_empty |> Bool.not)
;;
