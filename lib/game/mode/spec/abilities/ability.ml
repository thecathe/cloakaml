(** @canonical Game.Mode.Spec.Abilities.Ability *)

(** {1 Ability} *)

(** where ['a] is [data] *)
type 'a f = 'a -> unit

(** where [('a, 'b)] is [(trigger, data)] *)
type ('a, 'b) t = 'a * 'b f

module type S = sig
  type role
  type role_kind
  type role_alignment
  type trigger
  type trigger_kind
  type data

  (* type t = trigger * data f *)
  type nonrec t = (data, trigger) t

  exception RoleHasNoTrigger of (role * trigger)

  (* val role_trigger : trigger -> role -> *)
end

module type InputS = sig
  type role
  type role_kind
  type role_alignment
  type trigger
  type trigger_kind
  type data

  val abilty_of_role : role -> (trigger, data) t
end

module Make
    (RA : Roles.Alignment.S)
    (RK : Roles.Kind.S with type alignment = RA.t)
    (RR : Roles.Role.S with type kind = RK.t and type alignment = RA.t)
    (TK : Triggers.Kind.S)
    (TT : Triggers.Trigger.S with type kind = TK.t)
    (X :
       InputS
       with type role = RR.t
        and type role_kind = RK.t
        and type role_alignment = RA.t
        and type trigger = TT.t
        and type trigger_kind = TK.t) :
  S
  with type role = X.role
   and type role_kind = X.role_kind
   and type role_alignment = X.role_alignment
   and type trigger = X.trigger
   and type trigger_kind = X.trigger_kind
   and type data = X.data = struct
  type role = X.role
  type role_kind = X.role_kind
  type role_alignment = X.role_alignment
  type trigger = X.trigger
  type trigger_kind = X.trigger_kind

  (** {2 Ability Type} *)

  type data = X.data

  (* type t = trigger * data f *)
  type nonrec t = (data, trigger) t

  exception RoleHasNoTrigger of (role * trigger)
end
