-module(edownloader).

-export([start/0]).
-export([download_chunk/4, download_chunks/2]).
-export([check_agent/1]).

-record(ed_agent_state, {stat, part, num, worker, range, percent=0}).

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
        {ok, {_, Body}} ->
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
    Pid = spawn(fun() -> download_agent(#ed_agent_state{stat=wait, part=[], num=Num, range=Range}) end)
    , Pid ! {start, {UrlInfo, Range}}
    , Pid
.


download_agent(State) when is_record(State, ed_agent_state) ->
    Num = State#ed_agent_state.num
    , Part = State#ed_agent_state.part
    , Stat = State#ed_agent_state.stat
    , Worker = State#ed_agent_state.worker
    , Percent = State#ed_agent_state.percent
    , receive
        {start, {{Url, Tag, Modified}, R}} ->
            case download_chunk(Url, R, Tag, Modified) of
                {error, Type} ->
                    io:format("Error: ~p~n", [Type])
                    ,  download_agent(State#ed_agent_state{stat=fail});
                {ibrowse_req_id, Id} ->
                    download_agent(State#ed_agent_state{worker=Id})
            end;
        {finished, Pid} ->
            Pid ! {Stat, Num, {Part, Percent }};
        {stop, Pid} ->
            Pid ! {stopping, Num, Part}
            , exit(normal);
        {ibrowse_async_headers, Worker, "206", _Headers} ->
            download_agent(State#ed_agent_state{stat=wait});
        {ibrowse_async_response, Worker, Body} ->
            % TODO(jwall): log the percent complete for this agents work
            NewState = calc_percent(State#ed_agent_state{stat=in_progress, part=Part ++ Body})
            , download_agent(NewState);
        {ibrowse_async_response_end, Worker} ->
            download_agent(State#ed_agent_state{stat=ok, worker=Worker});
        {error, Type} ->
            io:format("Error: ~p~n", [Type])
            ,  download_agent(State#ed_agent_state{stat=fail});
        Msg ->
            io:format("Recieved Msg: ~p~n", [Msg])
    end
    , download_agent(State)
.

calc_percent(State) ->
    Total = length(State#ed_agent_state.part)
    , {Start, End} = State#ed_agent_state.range
    , P = trunc((Total / (End - Start)) * 100)
    , State#ed_agent_state{percent=P}
.

check_agent(Pid) ->
    Pid ! {finished, self()}
    , receive
        {ok, Num, {Body, _}} ->
            {ok, {Num, Body}};
        {fail, _, {Body, _}} ->
            {fail, Body};
        {Stat, Num, {_, _Percent}} when Stat == wait orelse Stat == in_progress ->
            {wait, Num}
    after
        5*1000 ->
            time_out
    end
.

download_chunk(Url, {Start, End}, Tag, Modified) ->
    Headers = edownload_util:set_headers_for_chunk("bytes", Start, End, Tag, Modified)
    , Options = [{stream_to, self()}]
    %, Options = []
    , Body = []
    , io:format("Headers: ~p~n", [Headers])
    %% TODO(jwall): configurable authentication
    , ibrowse:send_req(Url, Headers, get, Body, Options)
.

