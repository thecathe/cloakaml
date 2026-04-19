include Sig
open Types

exception RoleAbilityNotFound of (Roles.Role.t * Trigger.Kind.t)

module Make (X : sig
    val trigger : Trigger.t
    val roles : (Roles.Role.t * f) list
  end) : S = struct
  (** *)
  let trigger : Trigger.t = X.trigger

  (** *)
  let of_role (x : Roles.Role.t) : f =
    try List.find (fun (y, z) -> Roles.Role.equal x y) X.roles |> snd with
    | Not_found ->
      Printf.printf
        "role: %s\ntrigger: %s\nkind: %s\n"
        (Roles.Role.show x)
        (Trigger.show trigger)
        (Trigger.Kind.show (Trigger.kind trigger));
      raise (RoleAbilityNotFound (x, Trigger.kind trigger))
  ;;

  (** *)
  let get (x : Roles.Role.t) : map = trigger, of_role x
end

let make (trigger : Trigger.t) (xs : (Roles.Role.t * f) list) : (module S) =
  (module Make (struct
       let trigger = trigger
       let roles = xs
     end))
;;

(** {1 Role Ability Triggers} *)

let setup () : (module S) =
  make
    Setup
    (let baron : Round.t -> unit =
       fun ({ players; rolemap; _ } : Round.t) ->
       Players.replace_n_player_role_kinds 2 Townsfolk Outsider rolemap players
     in
     [ Baron, baron ])
;;

let start_of_game () : (module S) =
  make
    StartOfGame
    (let washerwoman = fun x -> Utils.todo __FUNCTION__ in
     let librarian = fun x -> Utils.todo __FUNCTION__ in
     let investigator = fun x -> Utils.todo __FUNCTION__ in
     let chef = fun x -> Utils.todo __FUNCTION__ in
     [ Washerwoman, washerwoman
     ; Librarian, librarian
     ; Investigator, investigator
     ; Chef, chef
     ])
;;

let perceived_as () : (module S) =
  make
    PerceivedAs
    (let recluse = fun x -> Utils.todo __FUNCTION__ in
     let spy = fun x -> Utils.todo __FUNCTION__ in
     [ Recluse, recluse; Spy, spy ])
;;

let each_day () : (module S) = make EachDay []

let each_night () : (module S) =
  make
    EachNight
    (let empath = fun x -> Utils.todo __FUNCTION__ in
     let fortune_teller = fun x -> Utils.todo __FUNCTION__ in
     let undertaker = fun x -> Utils.todo __FUNCTION__ in
     let monk = fun x -> Utils.todo __FUNCTION__ in
     let butler = fun x -> Utils.todo __FUNCTION__ in
     let poisoner = fun x -> Utils.todo __FUNCTION__ in
     let spy = fun x -> Utils.todo __FUNCTION__ in
     let imp = fun x -> Utils.todo __FUNCTION__ in
     [ Empath, empath
     ; FortuneTeller, fortune_teller
     ; Undertaker, undertaker
     ; Monk, monk
     ; Butler, butler
     ; Poisoner, poisoner
     ; Spy, spy
     ; Imp, imp
     ])
;;

let action () : (module S) =
  make
    Action
    (let slayer = fun x -> Utils.todo __FUNCTION__ in
     [ Slayer, slayer ])
;;

let on_death () : (module S) =
  make
    OnDeath
    (let ravenkeeper = fun x -> Utils.todo __FUNCTION__ in
     let saint = fun x -> Utils.todo __FUNCTION__ in
     let imp = fun x -> Utils.todo __FUNCTION__ in
     [ Ravenkeeper, ravenkeeper; Saint, saint; Imp, imp ])
;;

let on_nomination () : (module S) =
  make
    OnNomination
    (let virgin = fun x -> Utils.todo __FUNCTION__ in
     [ Virgin, virgin ])
;;

let always_active () : (module S) =
  make
    AlwaysActive
    (let solider = fun x -> Utils.todo __FUNCTION__ in
     [ Soldier, solider ])
;;

let end_day () : (module S) =
  make
    EndDay
    (let mayor = fun x -> Utils.todo __FUNCTION__ in
     [ Mayor, mayor ])
;;

let demon_dies () : (module S) =
  make
    DemonDies
    (let scarlet_woman = fun x -> Utils.todo __FUNCTION__ in
     [ ScarletWoman, scarlet_woman ])
;;

(** Shadowed *)
let of_trigger : Trigger.t -> (module S) = function
  | Setup -> setup ()
  | PerceivedAs -> perceived_as ()
  | StartOfGame -> start_of_game ()
  | EachNight -> each_night ()
  | EachDay -> each_day ()
  | OnDeath -> on_death ()
  | OnNomination -> on_nomination ()
  | Action -> action ()
  | AlwaysActive -> always_active ()
  | EndDay -> end_day ()
  | DemonDies -> demon_dies ()
;;

exception ProgrammerMessedUpTriggerModule of (Trigger.t * (module S))

(** shadowing with sanity check (avoid programmer error) *)
let of_trigger (x : Trigger.t) : (module S) =
  let module X : S = (val of_trigger x) in
  if Trigger.equal x X.trigger
  then (module X : S)
  else raise (ProgrammerMessedUpTriggerModule (x, (module X)))
;;

exception
  ProgrammerForgotRoleInTriggerModule of
    (Roles.Role.t * Trigger.t * Trigger.Kind.t)

(** shadowed *)
let get (x : Roles.Role.t) (y : Trigger.t) : map list =
  try
    let module X : S = (val of_trigger y) in
    [ X.get x ]
  with
  | RoleAbilityNotFound ((x, z) : Roles.Role.t * Trigger.Kind.t) ->
    raise (ProgrammerForgotRoleInTriggerModule (x, y, z))
;;

(** shadowing *)
let get (x : Roles.Role.t) : map list =
  let get = get x in
  match x with
  | Washerwoman | Librarian | Investigator | Chef -> get StartOfGame
  | Empath | FortuneTeller | Undertaker | Monk | Butler | Poisoner ->
    get EachNight
  | Ravenkeeper | Saint | Imp -> get OnDeath
  | Virgin -> get OnNomination
  | Slayer -> get Action
  | Soldier -> get AlwaysActive
  | Mayor -> get EndDay
  | ScarletWoman -> get DemonDies
  | Drunk | Recluse -> get PerceivedAs
  | Spy -> List.append (get PerceivedAs) (get PerceivedAs)
  | Baron -> get Setup
;;
