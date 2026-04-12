type t =
  { index : int
  ; role : Roles.t 
  ; mutable alive : bool
  }
[@@deriving show { with_path = false }]

let create (index : int) (role : Roles.t) : t = { index; role; alive = true }
let alive (x : t) : bool = x.alive
let dead (x : t) : bool = Bool.not (alive x)
let allied (a : t) (b : t) : bool = Roles.allied a.role b.role
let opposed (a : t) (b : t) : bool = Roles.opposed a.role b.role
let compare (a : t) (b : t) : int = Int.compare a.index b.index
let equal (a : t) (b : t) : bool = Int.equal a.index b.index
