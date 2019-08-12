-module(cache_store).

-export([init/0,
	 save/1,
	 lookup/1,
	 clear/0,
	 close/0,
	 test/0
	]).

name() ->
    "cache.dets".

init() ->
    case lists:member(name(), dets:all()) of
	true -> ok;
	false ->
	    {ok, _} = dets:open_file(name(), []),
	    dets:delete_all_objects(name()),
	    dets:insert(name(), {index, 1, 1}),
	    ok
    end.

save(Data) ->
    Key = erlang:md5(Data),
    [{index, Last, Next}] = dets:lookup(name(), index),
    dets:insert(name(), {Key, Next, Data}),
    case Next - Last of
	9 ->
	    dets:foldl(fun({index, _, _}, Acc) -> Acc;
			  ({XKey, XLast, _}, false) when XLast == Last ->
			       dets:delete(name(), XKey),
			       true;
			  (_, Acc) -> Acc
		       end, false, name()),
	    dets:insert(name(), {index, Last + 1, Next + 1});
	_ ->
	    dets:insert(name(), {index, Last, Next + 1})
    end,
    {ok, Key}.

lookup(Key) ->
    case dets:lookup(name(), Key) of
	[] -> not_found;
	[{_, _, Data}] -> {ok, Data}
    end.

clear() ->
    dets:delete_all_objects(name()),
    dets:insert(name(), {index, 1, 1}),
    ok.

close() ->
    dets:close(name()),
    ok.

test() ->

    ok = cache_store:init(),
    ok = cache_store:clear(),

    B01 = crypto:strong_rand_bytes(100),
    B02 = crypto:strong_rand_bytes(100),
    B03 = crypto:strong_rand_bytes(100),
    B04 = crypto:strong_rand_bytes(100),
    B05 = crypto:strong_rand_bytes(100),
    B06 = crypto:strong_rand_bytes(100),
    B07 = crypto:strong_rand_bytes(100),
    B08 = crypto:strong_rand_bytes(100),
    B09 = crypto:strong_rand_bytes(100),
    B10 = crypto:strong_rand_bytes(100),
    B11 = crypto:strong_rand_bytes(100),

    {ok, K01} = cache_store:save(B01),
    {ok, B01} = cache_store:lookup(K01),

    {ok, K02} = cache_store:save(B02),
    {ok, B02} = cache_store:lookup(K02),

    {ok, K03} = cache_store:save(B03),
    {ok, _K04} = cache_store:save(B04),
    {ok, _K05} = cache_store:save(B05),
    {ok, _K06} = cache_store:save(B06),
    {ok, _K07} = cache_store:save(B07),
    {ok, _K08} = cache_store:save(B08),
    {ok, _K09} = cache_store:save(B09),

    {ok, B01} = cache_store:lookup(K01),
    {ok, B02} = cache_store:lookup(K02),

    {ok, K10} = cache_store:save(B10),
    {ok, B10} = cache_store:lookup(K10),

    not_found = cache_store:lookup(K01),
    {ok, B02} = cache_store:lookup(K02),
    {ok, B03} = cache_store:lookup(K03),

    {ok, _K11} = cache_store:save(B11),

    not_found = cache_store:lookup(K01),
    not_found = cache_store:lookup(K02),
    {ok, B03} = cache_store:lookup(K03),

    ok = cache_store:clear(),

    ok.
