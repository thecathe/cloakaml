(** {1 Set of Players} *)

include Set.S with type elt = Player.t (** @closed *)

val show : t -> string

(** [add_role x ys] takes a {!Roles.t} [x] and makes a fresh {!Player.t} with [index] equal to {!cardinal}.
*)
val add_role : Roles.t -> t -> t

val create : elt list -> t

(** {2 Filter Functions} *)

val kinds : Roles.kind -> t -> t
val aligned : Roles.alignment -> t -> t
val random : ?f:(t -> t) -> t -> elt

(** {3 Player Fields} *)

(** {4 Status} *)

val alive : t -> t
val dead : t -> t
val poisoned : t -> t
val status : Player.Status.t -> t -> t

(** {4 Groups} *)

val allied : elt -> t -> t
val opposed : elt -> t -> t

exception NoPlayerWithRole of Roles.t

val group : Roles.group -> t -> t
val incl_self : (elt -> t -> t) -> elt -> t -> t

(** {3 Player Neighbours} *)

(** [neighbours x ys] returns the neighbours of [x] in [ys], i.e., those indexed either side of [x]. {b Note:} requires that [x] be in [ys]. {b Note:} if there is only one valid neighbour, then both {!Neighbours.left} and {!Neighbours.right} will refer to the same {!Player.t}. {b Note:} raises [NoNeighbour] in the event that [x] would be it's own neighbours.
*)
val neighbours : ?f:(t -> t) -> elt -> t -> Neighbours.t

val allied_neighbours : elt -> t -> Neighbours.t
val opposed_neighbours : elt -> t -> Neighbours.t
val alive_neighbours : elt -> t -> Neighbours.t
val dead_neighbours : elt -> t -> Neighbours.t

(** {2 Game Utils} *)

val with_active_abilities : t -> t
val with_phase_abilities : Phase.t -> t -> t

(** {3 Setup} *)

exception NoOldRole

(** [replace_kind old new map players] replaces the {{!Player.t.role}role} of one {{!Player.t}player} in [players] with {{!type:Roles.kind}kind} [old] is replaced with a new {{!Player.t.role}role} with {{!type:Roles.kind}kind} [new]. We use [map] to ensure that we don't introduce duplicate {{!Roles.t}roles} into the game, {i and} to that we can't re-add a {{!Roles.t}role} that has been removed.
*)
val replace_kind : Roles.kind -> Roles.kind -> bool Roles.Map.t -> t -> unit
