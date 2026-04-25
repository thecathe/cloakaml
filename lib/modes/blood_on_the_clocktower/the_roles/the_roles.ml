(** @canonical Modes.BloodOnTheClockTower.TheRoles *)

module Role = Role
module Kind = Kind
module Alignment = Alignment

module Make () =
  Mode.Spec.Roles.Make (Alignment) (Kind) (Role)
    (struct
      type role = Role.t
      type kind = Kind.t
      type alignment = Alignment.t

      let alignment_of_kind : kind -> alignment = function
        | Townsfolk | Outsider -> Good
        | Minion | Demon -> Evil
        | _ -> Neutral
      ;;

      let kind_of_role : role -> kind = function
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
