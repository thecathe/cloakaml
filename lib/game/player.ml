
(** {1 Player Type} *)

type t =
  { index : int
  ; mutable role : Roles.t 
  ; mutable alive : bool
  }
[@@deriving show { with_path = false }]

(** [create index role] ... *)
let create (index : int) (role : Roles.t) : t = { index; role; alive = true }

(** {2 Equality & Comparison} *)

let compare (a : t) (b : t) : int = Int.compare a.index b.index
let equal (a : t) (b : t) : bool = Int.equal a.index b.index

(** {2 Query Functions} *)

let alive (x : t) : bool = x.alive
let dead (x : t) : bool = Bool.not (alive x)
let allied (a : t) (b : t) : bool = Roles.allied a.role b.role
let opposed (a : t) (b : t) : bool = Roles.opposed a.role b.role

(** {2 Ability} *)

let ability (x:t) : Abilities.t = Abilities.make x.role
