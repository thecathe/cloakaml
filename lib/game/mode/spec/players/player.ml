(** @canonical Cloakaml.Game.Mode.Spec.Players.Player *)

type ('index, 'role, 'status, 'knowledge) t =
  { index : 'index
  ; mutable role : 'role
  ; mutable status : 'status
  ; knowledge : 'knowledge
  }

module type S = sig
  module Index : Index.S

  type index = Index.t

  module Roles : Roles.S

  type role = Roles.Role.t
  type role_kind = Roles.Kind.t
  type role_alignment = Roles.Alignment.t

  module Group :
    Group.S
    with type role = Roles.Role.t
     and type role_kind = Roles.Kind.t
     and type role_alignment = Roles.Alignment.t

  type group = Group.t
  type status
  type knowledge
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

module type InputS = sig
  (* type role
     type role_kind
     type role_alignment *)
end

module Make
    (I : Index.S)
    (R : Roles.S)
    (S : Status.S)
    (K : Knowledge.S)
    (G :
       Group.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t) :
  S
  with module Index = I (* and type index = I.t *)
   and module Roles = R
                      (* and type role = R.Role.t *)
                      (* and type role = G.role *)
                      (* and type Group.role = R.Role.t *)
                      (* and type Group.role = G.role *)
                      (* and type role_kind = R.Kind.t *)
                      (* and type role_kind = G.role_kind *)
                      (* and type Group.role_kind = R.Kind.t *)
                      (* and type Group.role_kind = G.role_kind *)
                      (* and type role_alignment = R.Alignment.t *)
                      (* and type role_alignment = G.role_alignment *)
                      (* and type Group.role_alignment = R.Alignment.t *)
                      (* and type Group.role_alignment = G.role_alignment *)
                      (* and module Roles.Role = R.Role *)
                      (* and type Roles.Role.t = R.Role.t *)
                      (* and module Roles.Kind = R.Kind *)
                      (* and type Roles.Kind.t = R.Kind.t *)
                      (* and module Roles.Alignment = R.Alignment *)
                      (* and type Roles.Alignment.t = R.Alignment.t *)
   and type status = S.t
   and type knowledge = K.t
   and module Group = G
(* and type group = G.t *)
(* and type Group.role_kind = R.Kind.t *)
(* and type Group.role_alignment = R.Alignment.t *) =
(* (X :
   InputS
   with type role = Role.t
   and type role_kind = Kind.t
   and type role_alignment = Alignment.t) *)
struct
  module Index = I

  type index = Index.t

  module Roles = R
  module Group = G

  type group = Group.t
  (* module Role = Roles.Role
     module Kind = Roles.Kind
     module Alignment = Roles.Alignment *)

  type role = Roles.Role.t
  type role_kind = Roles.Kind.t
  type role_alignment = Roles.Alignment.t
  type status = S.t
  type knowledge = K.t
  type nonrec t = (index, role, status, knowledge) t

  let create (index : index) (role : role) : t =
    { index; role; status = S.initial; knowledge = K.initial }
  ;;

  let index (x : t) : index = x.index
  let role (x : t) : role = x.role
  let status (x : t) : status = x.status
  let knowledge (x : t) : knowledge = x.knowledge

  let show (x : t) : string =
    Printf.sprintf
      "%s (%s, %s)"
      (Roles.Role.show x.role)
      (Index.show x.index)
      (S.show x.status)
  ;;

  let compare a b : int = Index.compare a.index b.index
  let equal a b : bool = Index.equal a.index b.index
  let hash x : int = Index.hash x.index
  let has_index (a : index) (x : t) : bool = Index.equal a x.index
  let has_role (a : role) (x : t) : bool = Roles.Role.equal a x.role
  let has_status (a : status) (x : t) : bool = S.equal a x.status
  let has_knowledge (a : knowledge) (x : t) : bool = K.equal a x.knowledge
  let has_group (a : group) (x : t) : bool = Group.is_role_of_group a x.role

  let replace_role (x : t) (a : role) : unit =
    x.role <- a;
    ()
  ;;
end
