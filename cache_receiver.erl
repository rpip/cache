%%%-------------------------------------------------------------------
%%% @author Yao Adzaku <yao@moonboots>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%% FSM STATES: start, raw_message, block_message
%%%
%%% see FSM handlers: cache_receiver_sync_fsm, cache_receiver_async_fsm
%%% while in block/raw message, expect more packets and add to temp store of packets
%%%   check if existing packets add up expected block size
%%%   If block size match,
%%%   save to store and enter read state, mark buffer as complete and save block to the store if in block state
%%%   if current size of data in buffer deosn't match blocksize, remain in state, incomplete
%%% if message is a key reference,
%%%   if exists in store, add to stream
%%%   else send missing reference message back
%%%   if client responds with missing info, enter block state and add
%%% if get_data
%%%   if current read buffer is complete, i,e all the packets receieved
%%%     then return {complete, data stream}
%%%     else return {incomplete, data_stream}
%%% @end
%%% Created : 30 May 2019 by Yao Adzaku <yao@moonboots>
%%%-------------------------------------------------------------------
-module(cache_receiver).

-behaviour(gen_server).

%% API
-export([start/0, stop/0, get_data/0, accept_loop/2, test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PORT, 12000).
-define(TCP_OPTIONS, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]).

-include("cache.hrl").

-record(state, {lsocket, handler}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

get_data() ->
   gen_server:call(?SERVER, get_data).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),

    io:format("starting cache store \n"),
    cache_store:init(),

    case gen_tcp:listen(?PORT, ?TCP_OPTIONS) of
   		{ok, LSocket} ->
            io:format("server started \n"),

            {ok, Handler} = cache_receiver_fsm:start_link(),
   			State = #state{lsocket = LSocket, handler=Handler},
   			{ok, accept(State)};
   		{error, Reason} ->
   			{stop, Reason}
	end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_data, _From, #state{lsocket = _Socket} = State) ->
    Response = gen_statem:call(State#state.handler, get_data),
    {reply, Response, State};

handle_call(stop, _From, #state{lsocket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, normal, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(accepted, State) ->
	{noreply, accept(State)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
accept(State = #state{lsocket=LSocket}) ->
    io:format("accepting connections \n"),
	proc_lib:spawn(?MODULE, accept_loop, [LSocket, State]),
	State.

%% TODO: document
accept_loop(LSocket, State) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
    % spawn a new process to handle connections, to avoid blocking
	gen_server:cast(?SERVER, accepted),
    loop(Socket, State#state.handler).

loop(Socket, Handler) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Response = cache_receiver_fsm:process(Handler, Data),
            case Response of
                <<?CACHE_MISSED_REFERENCE_MESSAGE:8, _Key/binary>> ->
                    gen_tcp_send(Socket, Response),
                    loop(Socket, Handler);
                <<?CACHE_REFERENCE_OK_MESSAGE:8, _Key:16/binary, _Rest/binary>> ->
                    gen_tcp_send(Socket, Response),
                    loop(Socket, Handler);
                _ ->
                    loop(Socket, Handler)
            end;
        {error, closed} ->
            ok
    end.

gen_tcp_send(Socket, <<Data:256/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Data),
    gen_tcp_send(Socket, Rest);
gen_tcp_send(Socket, Data) ->
    gen_tcp:send(Socket, Data).


test() ->
    {ok, RPid} = cache_receiver:start(),
    {ok, SPid} = cache_sender:start(),

    {complete, <<>>} = cache_receiver:get_data(),

    B01 = crypto:strong_rand_bytes(1024),
    B02 = crypto:strong_rand_bytes(1024),
    B03 = crypto:strong_rand_bytes(1024),

    ok = cache_sender:send_raw(B01),
    {ok,_K01} = cache_sender:send_block(B02),
    cache_sender:send_key(B02),
    ok = cache_sender:send_key(B01),
    {ok, _K02} = cache_sender:send_block(B01),
    ok = cache_sender:send_key(B03),

    timer:sleep(1000),
    FullData = <<B01/binary, B02/binary, B02/binary, B01/binary, B01/binary,
                 B03/binary>>,
    {complete, FullData} = cache_receiver:get_data(),

    % missing reference
    cache_sender:send_key(B03),
    timer:sleep(1000),
    FullData1 = <<FullData/binary, B03/binary>>,
    {complete, FullData1} = cache_receiver:get_data(),

    ok = cache_receiver:stop(),
    ok = cache_sender:stop(),

    false = is_process_alive(RPid),
    false = is_process_alive(SPid),

    ok.
