(** @canonical Game.Live.Round *)

module type S = sig
  type data

  val cache : data ref

  type 'a round =
    { state : data ref
    ; value : 'a
    }

  val get_data : data ref -> data round

  type 'a t = data ref -> 'a round

  val run : ?fresh:bool -> 'a t -> 'a
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a t) -> 'a t
  val state : (data -> data * 'a) -> data ref -> 'a round
  val sandbox : 'a t -> data ref -> 'a round
end

module Make (X : sig
    type data

    val initial : data
  end) : S with type data = X.data = struct
  (**  *)

  type data = X.data (** *)

  let cache : data ref = ref X.initial

  type 'a round =
    { state : data ref
    ; value : 'a
    }

  let get_data (x : data ref) : data round = { state = x; value = !x }

  type 'a t = data ref -> 'a round

  let run ?(fresh : bool = false) (x : 'a t) : 'a =
    let a : 'a round = x (if fresh then ref X.initial else cache) in
    a.value
  ;;

  let return (x : 'a) : 'a t = fun (state : data ref) -> { state; value = x }
  [@@inline always]
  ;;

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    fun (state : data ref) ->
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

  let state (f : data -> data * 'a) (x : data ref) : 'a round =
    let y, a = f !x in
    { state = ref y; value = a }
  ;;

  let sandbox (x : 'a t) (y : data ref) : 'a round =
    let store : data = !y in
    let a : 'a round = x y in
    { state = ref store; value = a.value }
  ;;
end
