(** @canonical Cloakaml.Game.Spec.Player *)

module Status = Status
module Knowledge = Knowledge

type ('index, 'role, 'status, 'knowledge) t =
  { index : 'index
  ; mutable role : 'role
  ; mutable status : 'status
  ; knowledge : 'knowledge
  }

module type S = sig
  module Id : Id.S

  type index = Id.t

  module Roles : Roles.S

  type role = Roles.role
  type role_kind = Roles.role_kind
  type role_alignment = Roles.role_alignment
  type group = Roles.group

  module Status : Status.S

  type status = Status.t

  module Knowledge : Knowledge.S with type index = index and type group = group

  type knowledge = Knowledge.t
  type nonrec t = (index, role, status, knowledge) t

  val create : index -> role -> t
  val index : t -> index
  val role : t -> role
  val status : t -> status
  val knowledge : t -> knowledge
  val show : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val has_index : index -> t -> bool
  val has_role : role -> t -> bool
  val has_status : status -> t -> bool
  val has_knowledge : knowledge -> t -> bool
  val has_group : group -> t -> bool
  val replace_role : t -> role -> unit
end

module Make (I : Id.S) (R : Roles.S) (S : Status.S) :
  S with module Id = I and module Roles = R and module Status = S = struct
  module Id = I

  type index = Id.t

  module Roles = R
  module Knowledge = Knowledge.Make (I) (R)

  type role = Roles.role
  type role_kind = Roles.role_kind
  type role_alignment = Roles.role_alignment
  type group = R.group

  module Status = S

  type status = Status.t
  type knowledge = Knowledge.t
  type nonrec t = (index, role, status, knowledge) t

  let create (index : index) (role : role) : t =
    { index; role; status = S.initial; knowledge = Knowledge.initial }
  ;;

  let index (x : t) : index = x.index
  let role (x : t) : role = x.role
  let status (x : t) : status = x.status
  let knowledge (x : t) : knowledge = x.knowledge

  let show (x : t) : string =
    Printf.sprintf
      "%s (%s, %s)"
      (Roles.Role.show x.role)
      (Id.show x.index)
      (S.show x.status)
  ;;

  let compare a b : int = Id.compare a.index b.index
  let equal a b : bool = Id.equal a.index b.index
  let hash x : int = Id.hash x.index
  let has_index (a : index) (x : t) : bool = Id.equal a x.index
  let has_role (a : role) (x : t) : bool = Roles.Role.equal a x.role
  let has_status (a : status) (x : t) : bool = S.equal a x.status

  let has_knowledge (a : knowledge) (x : t) : bool =
    Knowledge.equal a x.knowledge
  ;;

  let has_group (a : group) (x : t) : bool =
    Roles.Group.is_role_of_group a x.role
  ;;

  let replace_role (x : t) (a : role) : unit =
    x.role <- a;
    ()
  ;;
end
