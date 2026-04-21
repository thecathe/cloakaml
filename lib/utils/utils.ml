exception ToDo

let todo (s : string) : unit = Printf.printf "TODO: %s\n" s

(** [cata f x y] is {{:https://rocq-prover.org/doc/v9.1/api/rocq-runtime/Option/index.html#val-cata}Rocq.Option.cata}. If [y] is [None] returns [x], else [y] is [Some z] returns [f z].
*)
let cata (f : 'a -> 'b) (x : 'b) : 'a option -> 'b = function
  | None -> x
  | Some z -> f z
;;
