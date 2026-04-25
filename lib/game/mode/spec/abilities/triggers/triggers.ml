(** @canonical Game.Build.Abilities.Triggers *)

module Kind = Kind
module Trigger = Trigger

module type S = sig
  type role
  type role_kind
  type role_alignment
  type trigger
  type trigger_kind
  type t = trigger list
  type k = trigger_kind list

  val collect : unit -> t
  val of_kind : ?triggers:t -> trigger_kind -> t
  val of_kinds : ?triggers:t -> k -> t

  module Set : sig
    include Set.S with type elt = trigger

    val of_kind : ?triggers:t -> trigger_kind -> t
    val of_kinds : ?triggers:t -> k -> t
  end

  (* val of_role : role -> t *)
  (* val kinds_of_role : role -> k *)
end

(* module type InputS = sig
   type role
   type role_kind
   type role_alignment
   type trigger
   type trigger_kind

   (* val triggers_of_role : role -> trigger list *)
   end *)

module Make
    (RA : Roles.Alignment.S)
    (RK : Roles.Kind.S with type alignment = RA.t)
    (RR : Roles.Role.S with type kind = RK.t and type alignment = RA.t)
    (TK : Kind.S)
    (TT : Trigger.S with type kind = TK.t) :
  S
  with type role = RR.t
   and type role_kind = RK.t
   and type role_alignment = RA.t
   and type trigger = TT.t
   and type trigger_kind = TK.t =
(* (X :
   InputS
   with type role = RR.t
   and type role_kind = RK.t
   and type role_alignment = RA.t
   and type trigger = TT.t
   and type trigger_kind = TK.t) *)
(* : S
   with type role = X.role
   and type role_kind = X.role_kind
   and type role_alignment = X.role_alignment
   and type trigger = X.trigger
   and type trigger_kind = X.trigger_kind *)
struct
  type role = RR.t
  type role_kind = RK.t
  type role_alignment = RA.t
  type trigger = TT.t
  type trigger_kind = TK.t
  type t = trigger list
  type k = trigger_kind list

  let collect = TT.collect

  let of_kind ?(triggers : t = collect ()) (x : trigger_kind) : t =
    List.filter (TT.is_kind x) triggers |> List.sort_uniq TT.compare
  ;;

  let of_kinds ?(triggers : t = collect ()) (xs : k) : t =
    List.filter (TT.is_kinds xs) triggers |> List.sort_uniq TT.compare
  ;;

  module Set = struct
    include Set.Make (TT)

    let of_kind ?(triggers : t = collect () |> of_list) (x : trigger_kind) : t =
      filter (TT.is_kind x) triggers
    ;;

    let of_kinds ?(triggers : t = collect () |> of_list) (xs : k) : t =
      filter (TT.is_kinds xs) triggers
    ;;
  end

  (* let of_role : role -> t = X.triggers_of_role *)
  (* let kinds_of_role (x : role) : k = of_role x |> List.map TT.kind *)
end
