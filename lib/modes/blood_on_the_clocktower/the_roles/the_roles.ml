(** @canonical Modes.BloodOnTheClockTower.TheRoles *)

module Alignment : Spec.Roles.Role.Alignment.S with type t = Alignment.t =
  Spec.Roles.Role.Alignment.Make (Alignment)

module Kind :
  Spec.Roles.Role.Kind.S
  with type t = Kind.t
   and module Alignment = Alignment
   and type alignment = Alignment.t =
  Spec.Roles.Role.Kind.Make
    (Alignment)
    (struct
      include Kind

      type alignment = Alignment.t

      let alignment : t -> alignment = function
        | Townsfolk | Outsider -> Good
        | Minion | Demon -> Evil
        | _ -> Neutral
      ;;
    end)

module Role :
  Spec.Roles.Role.S
  with type t = Role.t
   and module Kind = Kind
   and type kind = Kind.t =
  Spec.Roles.Role.Make
    (Kind)
    (struct
      include Role

      type kind = Kind.t

      let kind : t -> kind = function
        (* townsfolk *)
        | Washerwoman -> Townsfolk
        | Librarian -> Townsfolk
        | Investigator -> Townsfolk
        | Chef -> Townsfolk
        | Empath -> Townsfolk
        | FortuneTeller -> Townsfolk
        | Undertaker -> Townsfolk
        | Monk -> Townsfolk
        | Ravenkeeper -> Townsfolk
        | Virgin -> Townsfolk
        | Slayer -> Townsfolk
        | Soldier -> Townsfolk
        | Mayor -> Townsfolk
        (* outsiders *)
        | Butler -> Outsider
        | Drunk -> Outsider
        | Recluse -> Outsider
        | Saint -> Outsider
        (* minions *)
        | Poisoner -> Minion
        | Spy -> Minion
        | ScarletWoman -> Minion
        | Baron -> Minion
        (* demons *)
        | Imp -> Demon
      ;;
    end)

module Build () : Spec.Roles.S = Spec.Roles.Make (Role)
