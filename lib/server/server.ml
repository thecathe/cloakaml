module Erlang = struct
  open Unix

  type t =
    { ic : in_channel
    ; oc : out_channel
    }

  let start () : t =
    (* let cwd = Printf.sprintf "%s/lib/server" (Sys.getcwd ()) in *)
    (* Printf.printf "(Server) cwd: %s\n" cwd; *)
    let ic, oc =
      "erl -noshell -pa _build/default/lib/server -s eserver start -s init stop"
      |> open_process
      (* "rebar3 -s ./server.erl start -s init stop" *)
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
end

let run () =
  print_endline "(Server) Hello, World!";
  let erl = Erlang.start () in
  let response = Erlang.request erl {|{"action":"ping"}|} in
  Printf.printf "Got: %s\n%!" response;
  ()
;;
