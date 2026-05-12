(** @canonical Game.Spec.Abilities.Ability *)

module Triggers = Triggers

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

  val triggers : triggers
end

module type InputS = sig
  include Data.S

  type triggers
  type trigger
  type trigger_kind

  val triggers : triggers
end

module Make
    (R : Roles.S)
    (T :
       Triggers.S
       with type role = R.role
        and type role_kind = R.role_kind
        and type role_alignment = R.role_alignment)
    (X :
       InputS
       with type triggers = T.t
        and type trigger = T.trigger
        and type trigger_kind = T.trigger_kind) :
  S
  with type role = R.role
   and type role_kind = R.role_kind
   and type role_alignment = R.role_alignment
   and type triggers = T.t
   and type trigger = T.trigger
   and type trigger_kind = T.trigger_kind = struct
  type role = R.role
  type role_kind = R.role_kind
  type role_alignment = R.role_alignment
  type triggers = T.t
  type trigger = T.trigger
  type trigger_kind = T.trigger_kind

  let triggers : triggers = X.triggers
end
