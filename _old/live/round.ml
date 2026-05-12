(** @canonical Game.Live.Round *)

module type S = sig
  val cache : Data.t ref

  type 'a round =
    { state : Data.t ref
    ; value : 'a
    }

  val get_data : Data.t ref -> Data.t round

  type 'a t = Data.t ref -> 'a round

  val run : ?fresh:bool -> 'a t -> 'a
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a t) -> 'a t
  val state : (Data.t -> Data.t * 'a) -> Data.t ref -> 'a round
  val sandbox : 'a t -> Data.t ref -> 'a round
end

module Make (X : sig
    val initial : Data.t
  end) : S = struct
  (**  *)

  let cache : Data.t ref = ref X.initial

  type 'a round =
    { state : Data.t ref
    ; value : 'a
    }

  let get_data (x : Data.t ref) : Data.t round = { state = x; value = !x }

  type 'a t = Data.t ref -> 'a round

  let run ?(fresh : bool = false) (x : 'a t) : 'a =
    let a : 'a round = x (if fresh then ref X.initial else cache) in
    a.value
  ;;

  let return (x : 'a) : 'a t = fun (state : Data.t ref) -> { state; value = x }
  [@@inline always]
  ;;

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    fun (state : Data.t ref) ->
    let a : 'a round = x state in
    f a.value a.state
  [@@inline always]
  ;;

  let map (f : 'a -> 'b) (x : 'a t) : 'b t = bind x (fun a -> return (f a))
  [@@inline always]
  ;;

  let product (x : 'a t) (y : 'b t) : ('a * 'b) t =
    bind x (fun a -> bind y (fun b -> return (a, b)))
  [@@inline always]
  ;;

  let rec iterate (i : int) (n : int) (acc : 'a) (f : int -> 'a -> 'a t) : 'a t =
    if i >= n
    then return acc
    else bind (f i acc) (fun acc' -> iterate (i + 1) n acc' f)
  ;;

  let state (f : Data.t -> Data.t * 'a) (x : Data.t ref) : 'a round =
    let y, a = f !x in
    { state = ref y; value = a }
  ;;

  let sandbox (x : 'a t) (y : Data.t ref) : 'a round =
    let store : Data.t = !y in
    let a : 'a round = x y in
    { state = ref store; value = a.value }
  ;;
end

let make (initial : Data.t) : (module S) =
  (module Make (struct
       let initial = initial
     end) : S)
;;
