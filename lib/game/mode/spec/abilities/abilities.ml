(** @canonical Game.Mode.Spec.Abilities *)

module Triggers = Triggers
module Ability = Ability

module type S = sig
  type role
  type role_kind
  type role_alignment
  type triggers
  type trigger
  type trigger_kind
end

module type InputS = sig
  type role
  type role_kind
  type role_alignment
  type triggers
  type trigger
  type trigger_kind


end

module Make (X : InputS) :
  S
  with type role = X.role
   and type role_kind = X.role_kind
   and type role_alignment = X.role_alignment
   and type triggers = X.triggers
   and type trigger = X.trigger
   and type trigger_kind = X.trigger_kind = struct
  type role = X.role
  type role_kind = X.role_kind
  type role_alignment = X.role_alignment
  type triggers = X.triggers
  type trigger = X.trigger
  type trigger_kind = X.trigger_kind

  (* let  *)
end
