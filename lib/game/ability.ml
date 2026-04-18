module Trigger = struct
  type t =
    | Setup
    | PerceivedAs
    | StartOfGame
    | EachNight
    | EachDay
    | OnDeath
    | OnNomination
    | Action
    | AlwaysActive
    | EndDay
    | DemonDies
  [@@deriving show { with_path = false }, eq, enum]
end

module Kind = struct
  type t =
    | Setup
    | StartOfGame
    | PhaseDependant
    | Passive
    | OnEvent
    | OneTimeUse
    | Conditional
  [@@deriving show { with_path = false }, eq]

  (* exception TriggerActiveOnPhase of Phase.t

     let kind_depends_on_phase (requires:Phase.t) : Phase.t option -> Kind.t = function
     | None -> raise (TriggerActiveOnPhase requires)
     | Some x -> Phase.equal x requires *)

  let of_trigger : Trigger.t -> t = function
    | Setup -> Setup
    | StartOfGame -> StartOfGame
    | PerceivedAs | AlwaysActive -> Passive
    | EachNight | EachDay -> PhaseDependant
    | OnDeath | OnNomination -> OnEvent
    | Action -> OneTimeUse
    | EndDay | DemonDies -> Conditional
  ;;

  let trigger_is (a : t) (b : Trigger.t) : bool = of_trigger b |> equal a
end

exception ToDo

let todo (s : string) : unit = Printf.printf "TODO: %s\n" s

module RoleAbility = struct
  type f = Round.t -> unit
  type map = Trigger.t * f

  module type S = sig
    val x : Roles.t
    val get : unit -> map list
    val triggers : unit -> Trigger.t list
    val has_kind : Kind.t -> bool
  end

  type t = (module S)

  let has_kind (x : Kind.t) (module X : S) : bool = X.has_kind x
  let filter_by_kind (x : Kind.t) : t list -> t list = List.filter (has_kind x)
  let role (module X : S) : Roles.t = X.x

  module Make (X : sig
      val x : Roles.t
      val abilities : map list
    end) : S = struct
    (** ... *)
    let x : Roles.t = X.x

    (** ... *)
    let get () : map list = X.abilities

    (** ... *)
    let triggers () : Trigger.t list = get () |> List.map fst

    (** ... *)
    let has_kind (x : Kind.t) : bool =
      triggers () |> List.exists (Kind.trigger_is x)
    ;;
  end

  exception RoleAbilityNotFound of (Roles.t * Kind.t)

  module OfRole = struct
    module type S = sig
      val trigger : Trigger.t
      val of_role : Roles.t -> f
      val get : Roles.t -> map
    end

    module Make (X : sig
        val trigger : Trigger.t
        val roles : (Roles.t * f) list
      end) : S = struct
      (** *)
      let trigger : Trigger.t = X.trigger

      (** *)
      let of_role (x : Roles.t) : f =
        try List.find (fun (y, z) -> Roles.equal x y) X.roles |> snd with
        | Not_found ->
          Printf.printf
            "role: %s\ntrigger: %s\nkind: %s\n"
            (Roles.show x)
            (Trigger.show trigger)
            (Kind.show (Kind.of_trigger trigger));
          raise (RoleAbilityNotFound (x, Kind.of_trigger trigger))
      ;;

      (** *)
      let get (x : Roles.t) : map = trigger, of_role x
    end

    let make (trigger : Trigger.t) (xs : (Roles.t * f) list) : (module S) =
      (module Make (struct
           let trigger = trigger
           let roles = xs
         end))
    ;;

    let setup () : (module S) =
      make
        Setup
        (let baron : Round.t -> unit =
           fun ({ players; rolemap; _ } : Round.t) ->
           Players.replace_n_player_role_kinds
             2
             Townsfolk
             Outsider
             rolemap
             players
         in
         [ Baron, baron ])
    ;;

    let start_of_game () : (module S) =
      make
        StartOfGame
        (let washerwoman = fun x -> todo __FUNCTION__ in
         let librarian = fun x -> todo __FUNCTION__ in
         let investigator = fun x -> todo __FUNCTION__ in
         let chef = fun x -> todo __FUNCTION__ in
         [ Washerwoman, washerwoman
         ; Librarian, librarian
         ; Investigator, investigator
         ; Chef, chef
         ])
    ;;

    let perceived_as () : (module S) =
      make
        PerceivedAs
        (let recluse = fun x -> todo __FUNCTION__ in
         let spy = fun x -> todo __FUNCTION__ in
         [ Recluse, recluse; Spy, spy ])
    ;;

    let each_day () : (module S) = make EachDay []

    let each_night () : (module S) =
      make
        EachNight
        (let empath = fun x -> todo __FUNCTION__ in
         let fortune_teller = fun x -> todo __FUNCTION__ in
         let undertaker = fun x -> todo __FUNCTION__ in
         let monk = fun x -> todo __FUNCTION__ in
         let butler = fun x -> todo __FUNCTION__ in
         let poisoner = fun x -> todo __FUNCTION__ in
         let spy = fun x -> todo __FUNCTION__ in
         let imp = fun x -> todo __FUNCTION__ in
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
        (let slayer = fun x -> todo __FUNCTION__ in
         [ Slayer, slayer ])
    ;;

    let on_death () : (module S) =
      make
        OnDeath
        (let ravenkeeper = fun x -> todo __FUNCTION__ in
         let saint = fun x -> todo __FUNCTION__ in
         let imp = fun x -> todo __FUNCTION__ in
         [ Ravenkeeper, ravenkeeper; Saint, saint; Imp, imp ])
    ;;

    let on_nomination () : (module S) =
      make
        OnNomination
        (let virgin = fun x -> todo __FUNCTION__ in
         [ Virgin, virgin ])
    ;;

    let always_active () : (module S) =
      make
        AlwaysActive
        (let solider = fun x -> todo __FUNCTION__ in
         [ Soldier, solider ])
    ;;

    let end_day () : (module S) =
      make
        EndDay
        (let mayor = fun x -> todo __FUNCTION__ in
         [ Mayor, mayor ])
    ;;

    let demon_dies () : (module S) =
      make
        DemonDies
        (let scarlet_woman = fun x -> todo __FUNCTION__ in
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
      ProgrammerForgotRoleInTriggerModule of (Roles.t * Trigger.t * Kind.t)

    (** shadowed *)
    let get (x : Roles.t) (y : Trigger.t) : map list =
      try
        let module X : S = (val of_trigger y) in
        [ X.get x ]
      with
      | RoleAbilityNotFound ((x, z) : Roles.t * Kind.t) ->
        raise (ProgrammerForgotRoleInTriggerModule (x, y, z))
    ;;

    (** shadowing *)
    let get (x : Roles.t) : map list =
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
  end

  let make (x : Roles.t) : t =
    (module Make (struct
         let x = x
         let abilities = OfRole.get x
       end) : S)
  ;;

  let get = make
  let of_player (x : Player.t) : t = make x.role

  exception NoneRoles

  let of_role_enum (x : int) : t =
    match Roles.of_enum x with None -> raise NoneRoles | Some x -> make x
  ;;

  let role_has_kind (x : Kind.t) (y : Roles.t) : bool = make y |> has_kind x
end

module RoleAbilities = struct
  type t = RoleAbility.t list

  let get ?(of_players : Players.t option = None) () : t =
    match of_players with
    | None -> List.init Roles.max RoleAbility.of_role_enum
    | Some players -> Players.to_list players |> List.map RoleAbility.of_player
  ;;

  let have_kind ?(of_players : Players.t option) (x : Kind.t) : Roles.t list =
    get ~of_players ()
    |> RoleAbility.filter_by_kind x
    |> List.map RoleAbility.role
  ;;
end

let player_has_kind (y : Kind.t) (x : Player.t) : bool =
  RoleAbility.role_has_kind y x.role
;;

let player_has_any_kinds (ys : Kind.t list) (x : Player.t) : bool =
  List.exists (fun y -> player_has_kind y x) ys
;;

let players_have_kind (y : Kind.t) : Player.t list -> Player.t list =
  List.filter (player_has_kind y)
;;

let players_have_kind (y : Kind.t) (xs : Players.t) : Players.t =
  Players.to_list xs |> players_have_kind y |> Players.of_list
;;

let players_have_any_kinds (ys : Kind.t list) (xs : Players.t) : Players.t =
  Players.to_list xs |> List.filter (player_has_any_kinds ys) |> Players.of_list
;;

let players_with_kinds_of_ability (ys : Kind.t list) (x : Round.t) : Players.t =
  players_have_any_kinds ys x.players
;;

module Map = struct
  module Map : Hashtbl.S with type key = Trigger.t = Hashtbl.Make (struct
      type t = Trigger.t [@@deriving show { with_path = false }]

      let hash x = Int.hash (Trigger.to_enum x)
      let equal a b = Int.equal (Trigger.to_enum a) (Trigger.to_enum b)
    end)

  include Map (** @closed *)

  type t' = RoleAbility.f t

  let add_all (map : t') (xs : RoleAbility.map list) : t' =
    xs |> List.to_seq |> Map.add_seq map;
    map
  ;;

  let of_role ?(map : t' = create 0) (x : Roles.t) : t' =
    let module R : RoleAbility.S = (val RoleAbility.make x) in
    R.get () |> add_all map
  ;;

  let make ?(map : t' = create 0) (x : Player.t) : t' = of_role ~map x.role

  let has_kind (map : t') (x : Kind.t) : bool =
    Map.to_seq map
    |> List.of_seq
    |> List.exists (fun (k, _) -> Kind.trigger_is x k)
  ;;

  let kind_opt (x : Kind.t) : key -> RoleAbility.f -> RoleAbility.f option =
    fun k v -> if Kind.trigger_is x k then Some v else None
  ;;

  let reduce_by_kind (map : t') (x : Kind.t) : unit =
    Map.filter_map_inplace (kind_opt x) map
  ;;
end

type t = Map.t' Player.Map.t

let map_player ?(map : t = Player.Map.create 0) (x : Player.t) : t =
  Player.Map.replace map x (Map.make x);
  map
;;

let map_players (xs : Players.t) : t =
  Players.cardinal xs
  |> Player.Map.create
  |> Players.fold (fun x map -> map_player ~map x) xs
;;

let kind_opt (x : Kind.t) : Player.t -> Map.t' -> Map.t' option =
  fun k v ->
  let map = Map.copy v in
  Map.reduce_by_kind map x;
  if Map.length map <= 0 then None else Some map
;;

let of_kind (y : Kind.t) (x : Round.t) : t =
  let map = map_players x.players in
  Player.Map.filter_map_inplace (kind_opt y) map;
  map
;;

let get_setup (x : Round.t) : t = of_kind Setup x
let players (map : t) = Player.Map.to_seq_keys map |> Players.of_seq
