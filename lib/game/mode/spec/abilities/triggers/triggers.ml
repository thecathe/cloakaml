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

module type InputS = sig
  type role
  type role_kind
  type role_alignment
  type trigger
  type trigger_kind

  (* val triggers_of_role : role -> trigger list *)
  val kind_of_trigger : trigger -> trigger_kind
end

module Make
    (* (RA : Roles.Alignment.S) *)
    (* (RK : Roles.Kind.S with type alignment = RA.t) *)
    (* (RR : Roles.Role.S with type kind = RK.t and type alignment = RA.t) *)
     (R : Roles.S)
    (K : Enum_type.InputS)
    (T : Enum_type.InputS)
    (* (T : Trigger.InputS with type kind = K.t) *)
     (X :
       InputS
       with type role = R.Role.t
        and type role_kind = R.Kind.t
        and type role_alignment = R.Alignment.t
        and type trigger = T.t
        and type trigger_kind = K.t) :
  S
  with type role = R.Role.t
   and type role_kind = R.Kind.t
   and type role_alignment = R.Alignment.t
   and type trigger = T.t
   and type trigger_kind = K.t =
(* (Kind : Kind.S) *)
(* (Trigger : Trigger.S with type kind = Kind.t) *)

(* (X :
   InputS
   with type role = RR.t
   and type role_kind = RK.t
   and type role_alignment = RA.t
   and type trigger = Trigger.t
   and type trigger_kind = Kind.t) *)
(* : S
   with type role = X.role
   and type role_kind = X.role_kind
   and type role_alignment = X.role_alignment
   and type trigger = X.trigger
   and type trigger_kind = X.trigger_kind *)
struct
  module Kind : Kind.S with type t = K.t = Kind.Make (K)

  module Trigger : Trigger.S with type t = T.t and type kind = Kind.t =
    Trigger.Make
      (Kind)
      (struct
        include T

        type kind = Kind.t

        let kind = X.kind_of_trigger
      end)

  type role = R.Role.t
  type role_kind = R.Kind.t
  type role_alignment = R.Alignment.t
  type trigger = Trigger.t
  type trigger_kind = Kind.t
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
