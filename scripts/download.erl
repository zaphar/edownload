#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ebin
-export([main/1]).

main(Args) ->
    Path = escript:script_name()
    , Dir = filename:dirname(Path)
    , code:add_path(Dir++"/../ebin")
    , io:format("Location: ~s~n", [Dir])
    , run(Args)
.

run([Url, Count]) ->
    run([Url, Count, edownload_util:filename_from_uri(Url)]);
run([Url, Count, Output]) ->
    io:format("Downloading ~s in ~p threads to ~s ~n", [Url, Count, Output])
    , edownloader:start()
    , Result = edownloader:download_chunks_to(Url, list_to_integer(Count), Output)
    , io:format("Saved: ~p~n", [Result])
.
