#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ebin
-export([main/1]).

main([Url, Count]) ->
    main([Url, Count, edownload_util:filename_from_uri(Url)]);
main([Url, Count, Output]) ->
    edownloader:start()
    , io:format("Downloading ~p in ~p threads", [Url, Count])
    , Content = edownloader:download_chunks(Url, list_to_integer(Count))
    , Result = file:write_file(Output, list_to_binary(Content))
    , io:format("Saved: ~p", [Result])
.
