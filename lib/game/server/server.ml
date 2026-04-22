module Erlang = struct
  open Unix

  type t =
    { ic : in_channel
    ; oc : out_channel
    }

  let start () : t =
    let ic, oc =
      open_process
        "erl -noshell -pa _build/default/lib/game/server -s eserver start"
    in
    { ic; oc }
  ;;

  let send t msg =
    output_string t.oc (msg ^ "\n");
    flush t.oc
  ;;

  let receive t =
    try input_line t.ic with End_of_file -> failwith "Erlang process closed"
  ;;

  let request t msg =
    send t msg;
    receive t
  ;;

  exception UnexpectedStart of string

  (** shadowed to handle opening message *)
  let start () : t =
    let erl = start () in
    match receive erl with "start" -> erl | x -> raise (UnexpectedStart x)
  ;;
end

let run () =
  print_endline "(Server) Hello, World!";
  let erl = Erlang.start () in
  print_endline "(Server) Started.";
  let response = Erlang.request erl {|{"action":"ping"}|} in
  Printf.printf "(Server) Got: %s\n%!" response;
  ()
;;
