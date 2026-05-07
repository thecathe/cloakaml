(** @canonical Game.Mode.Spec.Abilities.Ability *)

(** {1 Role Ability} *)

(** where ['a] is [data] *)
type 'a f = 'a -> unit

(** where [('a, 'b)] is [(trigger, data)] *)
(* type ('a, 'b) t = 'a * 'b f *)

module type S = sig
  type role
  type role_kind
  type role_alignment
  type triggers
  type trigger
  type trigger_kind
  type data

  (* type t = trigger * data f *)
  (* type nonrec t = (data, trigger) t *)
  val triggers : triggers

  exception TriggerNotFound of trigger

  val trigger : trigger -> data f
end

module type InputS = sig
  type role
  type role_kind
  type role_alignment
  type triggers
  type trigger
  type trigger_kind
  type data

  val triggers : triggers

  (* val abilty_of_role : role -> (trigger, data) t *)
  val of_trigger : trigger -> data f option
end

module Make
    (RA : Roles.Alignment.S)
    (RK : Roles.Kind.S with type alignment = RA.t)
    (RR : Roles.Role.S with type kind = RK.t and type alignment = RA.t)
    (TK : Triggers.Kind.S)
    (TT : Triggers.Trigger.S with type kind = TK.t)
    (TS :
       Triggers.S
       with type role = RR.t
        and type role_kind = RK.t
        and type role_alignment = RA.t
        and type trigger = TT.t
        and type trigger_kind = TK.t)
    (X :
       InputS
       with type role = RR.t
        and type role_kind = RK.t
        and type role_alignment = RA.t
        and type triggers = TS.t
        and type trigger = TT.t
        and type trigger_kind = TK.t) :
  S
  with type role = X.role
   and type role_kind = X.role_kind
   and type role_alignment = X.role_alignment
   and type triggers = X.triggers
   and type trigger = X.trigger
   and type trigger_kind = X.trigger_kind
   and type data = X.data = struct
  type role = X.role
  type role_kind = X.role_kind
  type role_alignment = X.role_alignment
  type triggers = X.triggers
  type trigger = X.trigger
  type trigger_kind = X.trigger_kind

  (** {2 Ability Type} *)

  type data = X.data

  (* type t = trigger * data f *)
  (* type nonrec t = (data, trigger) t *)

  let triggers : triggers = X.triggers

  exception TriggerNotFound of trigger

  let trigger (x : trigger) : data f =
    match X.of_trigger x with None -> raise (TriggerNotFound x) | Some y -> y
  ;;
end
