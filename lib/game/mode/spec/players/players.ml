(** @canonical Cloakaml.Game.Mode.Spec.Players *)

module type S = sig
  module Player : Player.S

  type player = Player.t
  type player_status = Player.status
  type player_knowledge = Player.knowledge
  type role = Player.role
  type role_kind = Player.role_kind
  type role_alignment = Player.role_alignment
  type group = Player.group

  module Selector : Selector.S with type player = Player.t and type t = player Utils.Selector.t

  module Map : Hashtbl.S with type key = player

  include Set.S with type elt = player [@@deriving show { with_path = false }]
  (** @closed *)

  val create : player list -> t
  val add_role : role -> t -> t

  exception CouldNotFindValidPlayer

  val random : ?f:(t -> t) -> t -> player
  val filterf : (player -> bool) -> t -> t
  val have_status : player_status -> t -> t
  val have_knowledge : player_knowledge -> t -> t
  val have_group : group -> t -> t
  val kind : role_kind -> t -> t
  val aligned : role_alignment -> t -> t
end

module Make (P : Player.S) : S with module Player = P and type elt = P.t =
struct
  module Player = P

  type role = P.role
  type role_kind = P.role_kind
  type role_alignment = P.role_alignment
  type group = P.group
  type player = P.t
  type player_status = P.status
  type player_knowledge = P.knowledge

  module Selector = Selector.Make (Player)


  (** {1 Map of Players} *)

  module Map : Hashtbl.S with type key = player = Hashtbl.Make (P)

  (** {1 Set of Players} *)

  include (Set.Make (P) : Set.S with type elt = player) [@@deriving
                                                          show
                                                            { with_path = false
                                                            }]
  (** @closed *)

  (** alias for [Set.of_list] *)
  let create : player list -> t = of_list

  (** [add_role x ys] takes a {!Roles.Role.t} [x] and makes a fresh {!Player.t} with [index] equal to {!cardinal}.
  *)
  let add_role (x : role) (ys : t) : t =
    add (P.create (cardinal ys |> P.Id.of_int) x) ys
  ;;

  exception CouldNotFindValidPlayer

  let random ?(f : (t -> t) option) (xs : t) : player =
    let xs : player list =
      Option.fold ~none:xs ~some:(fun f -> f xs) f |> to_list
    in
    Random.self_init ();
    try List.nth xs (Random.int (List.length xs)) with
    | Failure _ -> raise CouldNotFindValidPlayer
  ;;

  (** {2 Filter Functions} *)

  let filterf (f : player -> bool) (xs : t) : t =
    to_list xs |> List.filter f |> of_list
  ;;

  let have_status (a : player_status) : t -> t = filterf (P.has_status a)

  let have_knowledge (a : player_knowledge) : t -> t =
    filterf (P.has_knowledge a)
  ;;

  let have_group (group : group) : t -> t = filterf (P.has_group group)

  let kind (kind : role_kind) (xs : t) : t =
    (* filterf (fun (z : P.t) -> P.Roles.Kind.equal kind (P.Roles.Role.kind z.role)) xs *)
    have_group (Kind kind) xs
  ;;

  let aligned (alignment : role_alignment) (xs : t) : t =
    (* filterf
       (fun (z : P.t) -> P.Roles.Alignment.equal alignment (P.Roles.Role.alignment z.role))
       xs *)
    have_group (Alignment alignment) xs
  ;;
end
