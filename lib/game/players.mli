include Set.S with type elt = Player.t (** @closed *)

val add_role : Roles.t -> t -> t
val create : elt list -> t
val alive : t -> t
val dead : t -> t
val allied : elt -> t -> t
val opposed : elt -> t -> t

(** [neighbours x ys] returns the neighbours of [x] in [ys], i.e., those indexed either side of [x]. {b Note:} requires that [x] be in [ys]. {b Note:} if there is only one valid neighbour, then both {!Neighbours.left} and {!Neighbours.right} will refer to the same {!Player.t}. {b Note:} raises [NoNeighbour] in the event that [x] would be it's own neighbours.
*)
val neighbours : elt -> t -> Neighbours.t

val allied_neighbours : elt -> t -> Neighbours.t
val opposed_neighbours : elt -> t -> Neighbours.t
val alive_neighbours : elt -> t -> Neighbours.t
val dead_neighbours : elt -> t -> Neighbours.t
