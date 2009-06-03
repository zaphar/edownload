-module(edownloader).

-export([start/0]).
-export([download_chunk/4, download_chunks/2]).
-export([check_agent/1]).

start() -> ibrowse:start().

get_head(Url) ->
    {ok, "200", Headers, _} = ibrowse:send_req(Url, [], head)
    , {ok, Headers}
.

download_chunks(Url, Count) ->
     {ok, Headers} = get_head(Url)
     , {ok, Size} = edownload_util:get_size_from_header(Headers)
     , {ok, Tag} = edownload_util:get_etag_from_header(Headers)
     , {ok, Modified} = edownload_util:get_last_modified_from_header(Headers)
     , case edownload_util:accept_range(Headers) of
        false ->
            {fail, unsupported};
        _ ->
            RangeList = edownload_util:get_range_list(Size, Count)
            , Agents = download_agent({Url, Tag, Modified}, RangeList, 1)
            , accumulate_chunks(Agents)
     end
.

accumulate_chunks([]) ->
    [];
accumulate_chunks(Agents) when is_list(Agents) ->
    [H | T] = Agents
    , lists:flatten([accumulate_chunk(H) | accumulate_chunks(T)])
.

accumulate_chunk(Agent) ->
    case check_agent(Agent) of
        time_out ->
            accumulate_chunk(Agent);
        {ok, {Body, _}} ->
            Body;
        {wait, _} ->
            accumulate_chunk(Agent);
        Msg ->
            Msg
    end
.

download_agent(_, [], _) ->
    [];
download_agent(UrlInfo, RangeList, N) when is_list(RangeList) ->
    [H | T] = RangeList
    , [download_agent(UrlInfo, H, N) | download_agent(UrlInfo, T, N+1)];
download_agent(UrlInfo, Range, Num) when is_tuple(Range) ->
    Pid = spawn(fun() -> download_agent({wait, [], 0}) end)
    , Pid ! {start, {UrlInfo, Range, Num}}
    , Pid
.

download_agent({Stat, Part, Num}) ->
    receive
        {start, {{Url, Tag, Modified}, R, Count}} ->
            case download_chunk(Url, R, Tag, Modified) of
                {ok, "206", _Headers, Body} ->
                    download_agent({ok, Body, Count});
                {_, _, _Headers, Body} ->
                    download_agent({fail, Body, Count})
            end;
        {finished, Pid} ->
            Pid ! {Stat, Num, Part};
        {stop, Pid} ->
            Pid ! {stopping, Num, Part}
            , exit(normal)
    end
    , download_agent({Stat, Part, Num})
.

check_agent(Pid) ->
    Pid ! {finished, self()}
    , receive
        {ok, Body, Num} ->
            {ok, {Num, Body}};
        {fail, _, Num} ->
            {fail, Num};
        {wait, _, Num} ->
            {wait, Num}
    after
        5*1000 ->
            time_out
    end
.

download_chunk(Url, {Start, End}, Tag, Modified) ->
    ibrowse:send_req(Url
        , edownload_util:set_headers_for_chunk("bytes", Start, End, Tag, Modified)
        , get)
.

