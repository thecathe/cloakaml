(** @canonical Cloakaml.Game.Mode.Spec.Players.Player *)

type ('index, 'role, 'status, 'knowledge) t =
  { index : 'index
  ; mutable role : 'role
  ; mutable status : 'status
  ; knowledge : 'knowledge
  }

module type S = sig
  type index
  type role
  type role_kind
  type role_alignment
  type status
  type knowledge
  type group
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
  type role
  type role_kind
  type role_alignment
end

module Make
    (I : Index.S)
    (S : Status.S)
    (K : Knowledge.S)
    (R : Roles.S)
    (G :
       Group.S
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t)
    (X :
       InputS
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t) :
  S
  with type index = I.t
   and type role = R.Role.t
   and type role_kind = R.Kind.t
   and type role_alignment = R.Alignment.t
   and type status = S.t
   and type knowledge = K.t
   and type group = G.t = struct
  type index = I.t
  type role = R.Role.t
  type role_kind = R.Kind.t
  type role_alignment = R.Alignment.t
  type status = S.t
  type knowledge = K.t
  type group = G.t
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
      (R.Role.show x.role)
      (I.show x.index)
      (S.show x.status)
  ;;

  let compare a b : int = I.compare a.index b.index
  let equal a b : bool = I.equal a.index b.index
  let hash x : int = I.hash x.index
  let has_index (a : index) (x : t) : bool = I.equal a x.index
  let has_role (a : role) (x : t) : bool = R.Role.equal a x.role
  let has_status (a : status) (x : t) : bool = S.equal a x.status
  let has_knowledge (a : knowledge) (x : t) : bool = K.equal a x.knowledge
  let has_group (a : group) (x : t) : bool = G.is_role_of_group a x.role

  let replace_role (x : t) (a : role) : unit =
    x.role <- a;
    ()
  ;;
end
