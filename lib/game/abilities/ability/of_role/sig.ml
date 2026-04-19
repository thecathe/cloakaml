module type S = sig
  val trigger : Trigger.t
  val of_role : Roles.Role.t -> Types.f
  val get : Roles.Role.t -> Types.map
end
  