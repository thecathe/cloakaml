(** @canonical Game.Spec.Abilities.Ability.Triggers *)

module Trigger = Trigger

module type S = sig
  type role
  type role_kind
  type role_alignment

  module Trigger : Trigger.S

  type trigger = Trigger.t
  type trigger_kind = Trigger.kind
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

module Make (R : Roles.S) (T : Trigger.S) :
  S
  with type role = R.role
   and type role_kind = R.role_kind
   and type role_alignment = R.role_alignment
   and module Trigger = T = struct
  type role = R.role
  type role_kind = R.role_kind
  type role_alignment = R.role_alignment

  module Trigger = T

  type trigger = Trigger.t
  type trigger_kind = Trigger.kind
  type t = trigger list
  type k = trigger_kind list

  let collect = Trigger.collect

  let of_kind ?(triggers : t = collect ()) (x : trigger_kind) : t =
    List.filter (Trigger.is_kind x) triggers |> List.sort_uniq Trigger.compare
  ;;

  let of_kinds ?(triggers : t = collect ()) (xs : k) : t =
    List.filter (Trigger.is_kinds xs) triggers |> List.sort_uniq Trigger.compare
  ;;

  module Set = struct
    include Set.Make (Trigger)

    let of_kind ?(triggers : t = collect () |> of_list) (x : trigger_kind) : t =
      filter (Trigger.is_kind x) triggers
    ;;

    let of_kinds ?(triggers : t = collect () |> of_list) (xs : k) : t =
      filter (Trigger.is_kinds xs) triggers
    ;;
  end

  (* let of_role : role -> t = X.triggers_of_role *)
  (* let kinds_of_role (x : role) : k = of_role x |> List.map Trigger.kind *)
end
