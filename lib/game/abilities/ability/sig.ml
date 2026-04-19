

module type S = sig
  val x : Roles.Role.t
  val get : unit -> Types.map list
  val triggers : unit -> Trigger.t list
  val has_kind : Trigger.Kind.t -> bool
end

type t = (module S)
