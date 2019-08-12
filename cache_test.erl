-module(cache_test).
-compile(export_all).

run_all() ->

    ok = test1(),
    ok = test2(),
    ok = test3(),
    ok = test4(),

    ok.

test1() ->

    {ok, RPid} = cache_receiver:start(),
    {ok, SPid} = cache_sender:start(),

    {complete, <<>>} = cache_receiver:get_data(),
    true = cache_sender:is_done(),

    ok = cache_receiver:stop(),
    ok = cache_sender:stop(),

    false = is_process_alive(RPid),
    false = is_process_alive(SPid),

    ok.


test2() ->

    {ok, _} = cache_receiver:start(),
    {ok, _} = cache_sender:start(),

    {complete, <<>>} = cache_receiver:get_data(),
    true = cache_sender:is_done(),

    B01 = crypto:strong_rand_bytes(1024),
    B02 = crypto:strong_rand_bytes(1024),
    B03 = crypto:strong_rand_bytes(1024),
    B04 = crypto:strong_rand_bytes(1024),
    B05 = crypto:strong_rand_bytes(1024),
    B06 = crypto:strong_rand_bytes(1024),

    {ok, _K01} = cache_sender:send_block(B01),
    {ok, _K02} = cache_sender:send_block(B02),
    {ok, _K03} = cache_sender:send_block(B03),
    {ok, _K04} = cache_sender:send_block(B04),
    {ok, _K05} = cache_sender:send_block(B05),
    {ok, _K06} = cache_sender:send_block(B06),

    FullData = <<B01/binary, B02/binary, B03/binary, B04/binary, B05/binary, B06/binary>>,

    timer:sleep(1000),
    {complete, FullData} = cache_receiver:get_data(),
    true = cache_sender:is_done(),

    ok = cache_receiver:stop(),
    ok = cache_sender:stop(),

    ok.


test3() ->

    {ok, _} = cache_receiver:start(),
    {ok, _} = cache_sender:start(),

    {complete, <<>>} = cache_receiver:get_data(),
    true = cache_sender:is_done(),

    B01 = crypto:strong_rand_bytes(1024),
    B02 = crypto:strong_rand_bytes(1024),
    B03 = crypto:strong_rand_bytes(1024),
    B04 = crypto:strong_rand_bytes(1024),
    B05 = crypto:strong_rand_bytes(1024),
    B06 = crypto:strong_rand_bytes(1024),

    {ok, _K01} = cache_sender:send_block(B01),
    {ok, _K02} = cache_sender:send_block(B02),
    ok         = cache_sender:send_key(B01),
    {ok, _K03} = cache_sender:send_block(B03),
    {ok, _K04} = cache_sender:send_block(B04),
    ok         = cache_sender:send_key(B02),
    {ok, _K05} = cache_sender:send_block(B05),
    {ok, _K06} = cache_sender:send_block(B06),
    ok         = cache_sender:send_key(B01),

    FullData = <<B01/binary, B02/binary, B01/binary, B03/binary, B04/binary,
		B02/binary, B05/binary, B06/binary, B01/binary>>,

    timer:sleep(1000),
    {complete, FullData} = cache_receiver:get_data(),
    true = cache_sender:is_done(),

    ok = cache_receiver:stop(),
    ok = cache_sender:stop(),

    ok.


test4() ->

    {ok, _} = cache_receiver:start(),
    {ok, _} = cache_sender:start(),

    {complete, <<>>} = cache_receiver:get_data(),
    true = cache_sender:is_done(),

    B01 = crypto:strong_rand_bytes(1024),
    B02 = crypto:strong_rand_bytes(1024),
    B03 = crypto:strong_rand_bytes(1024),
    B04 = crypto:strong_rand_bytes(1024),
    B05 = crypto:strong_rand_bytes(1024),
    B06 = crypto:strong_rand_bytes(1024),
    B07 = crypto:strong_rand_bytes(1024),
    B08 = crypto:strong_rand_bytes(1024),
    B09 = crypto:strong_rand_bytes(1024),
    B10 = crypto:strong_rand_bytes(1024),
    B11 = crypto:strong_rand_bytes(1024),
    B12 = crypto:strong_rand_bytes(1024),
    B13 = crypto:strong_rand_bytes(1024),
    B14 = crypto:strong_rand_bytes(1024),
    B15 = crypto:strong_rand_bytes(1024),
    B16 = crypto:strong_rand_bytes(1024),

    {ok, _K01} = cache_sender:send_block(B01),
    {ok, _K02} = cache_sender:send_block(B02),
    ok         = cache_sender:send_key(B01),
    {ok, _K03} = cache_sender:send_block(B03),
    {ok, _K04} = cache_sender:send_block(B04),
    ok         = cache_sender:send_key(B02),
    {ok, _K05} = cache_sender:send_block(B05),
    {ok, _K06} = cache_sender:send_block(B06),
    ok         = cache_sender:send_key(B01),
    {ok, _K07} = cache_sender:send_block(B07),
    {ok, _K08} = cache_sender:send_block(B08),
    {ok, _K09} = cache_sender:send_block(B09),
    {ok, _K10} = cache_sender:send_block(B10),
    {ok, _K11} = cache_sender:send_block(B11),
    {ok, _K12} = cache_sender:send_block(B12),
    {ok, _K13} = cache_sender:send_block(B13),
    ok         = cache_sender:send_key(B01),
    {ok, _K14} = cache_sender:send_block(B14),
    {ok, _K15} = cache_sender:send_block(B15),
    {ok, _K16} = cache_sender:send_block(B16),

    FullData = <<B01/binary, B02/binary, B01/binary, B03/binary, B04/binary,
		B02/binary, B05/binary, B06/binary, B01/binary, B07/binary,
		B08/binary, B09/binary, B10/binary, B11/binary, B12/binary,
		B13/binary, B01/binary, B14/binary, B15/binary, B16/binary>>,

    timer:sleep(1000),
    {complete, FullData} = cache_receiver:get_data(),
    true = cache_sender:is_done(),

    ok = cache_receiver:stop(),
    ok = cache_sender:stop(),

    ok.
