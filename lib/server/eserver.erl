-module(eserver).
-export([start/0]).

start() ->
  io:format ("start~n"),
  loop().

loop() ->
    case io:get_line("") of
        eof -> ok;
        Line ->
            %% parse message
            io:format("~s~n", [handle(Line)]),
            loop()
    end.

handle("{\"action\":\"ping\"}") ->
    "{\"result\":\"pong\"}";

handle(Other) ->
    io_lib:format("{\"error\":\"unknown\",\"input\":~p}", [Other]).