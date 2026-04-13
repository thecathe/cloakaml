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
let create (xs : elt list) : t = of_list xs
let alive : t -> t = filter Player.alive
let dead : t -> t = filter Player.dead
let allied (x : elt) : t -> t = filter (Player.allied x)
let opposed (x : elt) : t -> t = filter (Player.opposed x)

let neighbours (x : elt) (ys : t) : Neighbours.t =
  to_list ys |> Neighbours.find x
;;

let filter_neighbours (f : t -> t) (x : elt) (ys : t) : Neighbours.t =
  f ys |> add x |> neighbours x
;;

let allied_neighbours (x : elt) : t -> Neighbours.t =
  filter_neighbours (allied x) x
;;

let opposed_neighbours (x : elt) : t -> Neighbours.t =
  filter_neighbours (opposed x) x
;;

let alive_neighbours : elt -> t -> Neighbours.t = filter_neighbours alive
let dead_neighbours : elt -> t -> Neighbours.t = filter_neighbours dead
