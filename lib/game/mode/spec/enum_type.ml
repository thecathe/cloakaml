(** @canonical Cloakaml.Game.Mode.Spec.EnumType *)

(** {1 unused} *)

exception EnumKindTodo

module type S = sig
  type t [@@deriving show { with_path = false }, eq, enum]

  exception EnumOutOfBounds of int

  val of_int : int -> t
  val next : ?wrap:bool -> ?amount:int -> t -> t
  val prev : ?wrap:bool -> ?amount:int -> t -> t
  val collect : unit -> t list
end

module type InputS = sig
  type t [@@deriving show { with_path = false }, eq, enum]
end

module Make (X : InputS) : S with type t = X.t = struct
  include X

  exception EnumOutOfBounds of int

  let of_int (n : int) : t =
    match of_enum n with Some x -> x | None -> raise (EnumOutOfBounds n)
  ;;

  let next ?(wrap : bool = true) ?(amount : int = 1) (x : t) : t =
    let n : int = to_enum x in
    if Int.equal n max
    then if wrap then raise (EnumOutOfBounds n) else of_int 0
    else of_int (n + amount)
  ;;

  let prev ?(wrap : bool = true) ?(amount : int = 1) (x : t) : t =
    let n : int = to_enum x in
    if Int.equal n min
    then if wrap then raise (EnumOutOfBounds n) else of_int max
    else of_int (n - amount)
  ;;

  let collect () : t list = List.init (max + 1) of_int
end
