-module(edownloader).

-include_lib("kernel/include/file.hrl").

-export([start/0]).
-export([download_chunk/4, download_chunks/2, download_chunks_to/3, download_chunks_to/2]).
-export([check_agent/1]).

-record(ed_agent_state, {stat, total=0, num=0, worker, range, percent=0, buffer, tries=1, url, tag, modified, marker=0}).

start() -> ibrowse:start().

get_head(Url) ->
    {ok, "200", Headers, _} = ibrowse:send_req(Url, [], head)
    , {ok, Headers}
.

download_chunks_to(Url, Count) ->
    File = edownload_util:filename_from_uri(Url)
    , download_chunks_to(Url, Count, File)
.

download_chunks_to(Url, Count, File) ->
    {ok, IoDevice} = file:open(File, [write])
    , Buffers = download_chunks(Url, Count)
    , [cat_file_to(IoDevice, B) || B <- Buffers]
.

cat_file_to(Dest, Source) ->
    , io:format("Dest  : ~p~n", [Dest])
    , io:format("Source: ~p~n", [Source])
    , {ok, FileName} = file:pid2name(Source)
    , io:format("file name: ~p", [FileName])
    , file:close(Source)
    , {ok, Data} =  file:read_file(FileName)
    , io:format("file was read: ~p", [FileName])
    , file:write(Dest, Data)
    , file:delete(FileName)
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
            , accumulate_buffers(Agents)
     end
.

accumulate_buffers([]) ->
    [];
accumulate_buffers(Agents) when is_list(Agents) ->
    [H | T] = Agents
    , [accumulate_buffer(H) | accumulate_buffers(T)]
.

accumulate_buffer(Agent) ->
    case check_agent(Agent) of
        time_out ->
            accumulate_buffer(Agent);
        {ok, {_Num, Buffer}} ->
            Buffer;
        {wait, _} ->
            accumulate_buffer(Agent);
        {fail, Buffer} ->
            Buffer
    end
.

download_agent(_, [], _) ->
    [];
download_agent(UrlInfo, RangeList, N) when is_list(RangeList) ->
    [H | T] = RangeList
    , [download_agent(UrlInfo, H, N) | download_agent(UrlInfo, T, N+1)];
download_agent(UrlInfo, Range, Num) when is_tuple(Range) ->
    Pid = spawn(fun() -> download_agent(#ed_agent_state{stat=wait, total=0, num=Num, range=Range}) end)
    , Pid ! {start, {UrlInfo, Range}}
    , Pid
.


download_agent(State) when is_record(State, ed_agent_state) ->
    Num = State#ed_agent_state.num
    , Stat = State#ed_agent_state.stat
    , Total = State#ed_agent_state.total
    , Marker = State#ed_agent_state.marker
    , Worker = State#ed_agent_state.worker
    , Percent = State#ed_agent_state.percent
    , Buffer = State#ed_agent_state.buffer
    , {Start, End} = State#ed_agent_state.range
    , receive
        {start, {{Url, Tag, Modified}, R}} ->
            FileName = integer_to_list(erlang:phash2(make_ref()))
            , {ok, File} = file:open(FileName, [write, read])
            , NewState = State#ed_agent_state{url=Url, tag=Tag, modified=Modified, range=R, buffer=File, marker=Start}
            , start_chunk_download(NewState);
        {finished, Pid} ->
            Pid ! {Stat, Num, {Buffer, Percent }};
        {stop, Pid} ->
            Pid ! {stopping, Num, Buffer}
            , exit(normal);
        {ibrowse_async_headers, Worker, "206", _Headers} ->
            download_agent(State#ed_agent_state{stat=wait});
        {ibrowse_async_response, Worker, {error, Type}} ->
            Tries = State#ed_agent_state.tries
            , io:format("Warning: ~p. Tries: ~p~n", [Type, Tries])
            , case (State#ed_agent_state.marker > End) of
                true ->
                    erlang:error({attempt_to_exceed_download_size, {marker, State#ed_agent_state.marker}});
                false ->
                    start_chunk_download(State#ed_agent_state{tries=Tries + 1})
            end;
        {ibrowse_async_response, Worker, Body} ->
            file:write(Buffer, Body)
            , L = length(Body)
            , NewState = calc_percent(State#ed_agent_state{stat=in_progress, total=Total+L,
                marker=Marker+L})
            %, io:format("~nPiece #~p      just downloaded: ~p bytes~n", [Num, L]) 
            %, io:format("Piece #~p           marker was: ~p bytes~n", [Num, Marker]) 
            %, io:format("Piece #~p            marker is: ~p bytes~n", [Num, NewState#ed_agent_state.marker]) 
            %, io:format("Piece #~p     total downloaded: ~p bytes~n", [Num, NewState#ed_agent_state.total]) 
            %, io:format("Piece #~p total to download is: ~p~n", [Num, End ja[- Start])
            %, io:format("Piece #~p is ~p% downloaded~n~n", [Num, NewState#ed_agent_state.percent])
            , download_agent(NewState);
        {ibrowse_async_response_end, Worker} ->
            download_agent(State#ed_agent_state{stat=ok, worker=Worker});
        {error, Type} ->
            io:format("Error during download: ~p~n", [Type])
            ,  download_agent(State#ed_agent_state{stat=fail});
        Msg ->
            io:format("Recieved Msg: ~p~n", [Msg])
    end
    , download_agent(State)
.

start_chunk_download(State) ->
    Url = State#ed_agent_state.url
    , {Start, End} = State#ed_agent_state.range
    , Tag = State#ed_agent_state.tag
    , Modified = State#ed_agent_state.modified
    , Total = State#ed_agent_state.total
    , Marker = State#ed_agent_state.marker
    , R = case Marker of
        0 ->
            {Start, End};
        Start ->
            {Start, End};
        NewStart when (NewStart + 1) < End ->
            {NewStart+1, End}
    end
    , case State#ed_agent_state.total > (End - Start) of
        true ->
            io:format("Warning already at end of chunk: ~p~n", [State#ed_agent_state.num])
            , io:format("Warning ~p Total so far        : ~p~n", [State#ed_agent_state.num, Total])
            , io:format("Warning ~p Marker so far       : ~p~n", [State#ed_agent_state.num, Marker])
            , download_agent(State);
        _ ->
            case download_chunk(Url, R, Tag, Modified) of
                {error, Type} ->
                    io:format("Error starting download: ~p~n", [Type])
                    ,  download_agent(State#ed_agent_state{stat=fail});
                {ibrowse_req_id, Id} ->
                    io:format("Ranges:  needed [~p] requested [~p]~n", [{Start, End}, R])
                    , download_agent(State#ed_agent_state{worker=Id})
            end
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

check_agent(Pid) ->
    Pid ! {finished, self()}
    , receive
        {ok, Num, {Buffer, _}} ->
            {ok, {Num, Buffer}};
        {fail, _, {Buffer, _}} ->
            {fail, Buffer};
        {Stat, Num, {_, _Percent}} when Stat == wait orelse Stat == in_progress ->
            {wait, Num}
    after
        5*1000 ->
            time_out
    end
.

calc_percent(State) ->
    Total = State#ed_agent_state.total
    , {Start, End} = State#ed_agent_state.range
    , P = trunc((Total / (End - Start)) * 100)
    , State#ed_agent_state{percent=P}
.

