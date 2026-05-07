(** @canonical Cloakaml.Game.Mode.Spec.Player.Knowledge *)

type ('index, 'group) t = ('index, 'group) datum list

and ('index, 'group) datum =
  { players : 'index Utils.Selector.t
  ; group : 'group
  }

module type S = sig
  type index
  type group

  type nonrec t = (index, group) t
  and datum = (index, group) datum

  val initial : t
  val equal : t -> t -> bool
end

module Make (I : Index.S) (R : Roles.S) :
  S
  with type index = I.t
   and type group = R.group
   and type t = (I.t, R.group) t
   and type datum = (I.t, R.group) datum = struct
  type index = I.t
  type group = R.group

  type nonrec t = (index, group) t
  and datum = (index, group) datum

  let initial : t = []

  let equal_datum (a : datum) (b : datum) : bool =
    (* Selector.equal (I.equal) a.players b.players *)
    (* && *)
    R.Group.equal a.group b.group
  ;;

  let equal (a : t) (b : t) : bool = List.equal equal_datum a b
end
