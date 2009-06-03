-module(edownload_util).
-compile(export_all).


accept_range(Headers) ->
    case get_header_value("Accept-Ranges", Headers) of
        {ok, Type} ->
            {ok, Type};
        {err, {no_such_value, _}} ->
            false
    end
.

get_last_modified_from_header(Headers) ->
    get_header_value("Last-Modified", Headers)
.

get_etag_from_header(Headers) ->
    get_header_value("ETag", Headers)
.

get_size_from_header(Headers) ->
    {ok, Size} = get_header_value("Content-Length", Headers)
    , {Int, _} = string:to_integer(Size)
    , {ok, Int}
.

get_header_value(Key, Headers) when is_atom(Key) ->
    get_header_value(atom_to_list(Key), Headers);
get_header_value(Key, Headers) when is_list(Key) ->
    case lists:keysearch(Key, 1, Headers) of
        {value, {_, Value}} ->
            {ok, Value};
        false ->
            {err, {no_such_value, Key}}
    end
.

get_range_list(Total, Count) ->
    {ChunkSize, Rem} = get_chunk_size(Total, Count)
    , F = fun(I) ->
        Offset = get_offset_for_chunk(ChunkSize, I)
        , {Offset, Offset+ChunkSize-1}
    end
    , List = lists:seq(1,Count-1) 
    , Ranges = [ F(I) || I  <- List ]
    , {Last, _} = F(Count)
    , Ranges ++ [{Last, (Last+ChunkSize+Rem)}]
.

get_offset_for_chunk(Chunk, Num) ->
    (Chunk * (Num -1))
.

get_chunk_size(Total, Count) ->
    {trunc(Total / Count), Total rem Count}
.

set_headers_for_chunk(Type, Offset, End, Tag, Date)  when is_integer(Offset) and is_integer(End) ->
    [
        {"Range", Type ++ "=" ++ integer_to_list(Offset) ++ "-" ++ integer_to_list(End)}
        , {"ETag", Tag}
        , {"Unless-Modified-Since", Date}
        , {"If-Range", Tag}
    ]
.

set_headers_for_rest(Type, Offset, Tag, Date) ->
    [
        {"Range", Type ++ "=" ++ integer_to_list(Offset) ++ "-"}
        , {"ETag", Tag}
        , {"Unless-Modified-Since", Date}
        , {"If-Range", Tag}
    ]
.
