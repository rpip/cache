%%%-------------------------------------------------------------------
%%% @author Yao Adzaku <yao@moonboots>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%% Blocking FSM API implementation.
%%% @end
%%% Created : 30 May 2019 by Yao Adzaku <yao@moonboots>
%%%-------------------------------------------------------------------
-module(cache_receiver_fsm).

-behaviour(gen_statem).

%% API
-export([start_link/0, start/0, stop/0, process/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([start/3, raw_message/3, block_message/3]).

-define(SERVER, ?MODULE).

-include("cache.hrl").

%% buffer is accumulator for current block/raw packets
-record(buffer, {blocksize, packets=[]}).
-record(state, {stream=[], buffer=#buffer{}, pending_refs=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
   start_link().

stop() ->
    gen_statem:stop(?SERVER).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  gen_statem:init_result(atom()).
init([]) ->
    process_flag(trap_exit, true),
    {ok, start, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
%~ state_name(_EventType, _EventContent, State) ->
%~     NextStateName = next_state,
%~     {next_state, NextStateName, State}.
-spec start(gen_statem:event_type(),
            Msg :: term(),
            Data :: term()) ->
            gen_statem:event_handler_result(atom()).

start({call, Caller}, <<?CACHE_REFERENCE_MESSAGE:8, Key/binary>>, State) ->
    do_reference_message(Key, Caller, State);
start({call, Caller}, get_data, State) ->
    get_data(Caller, State);
start({call, Caller}, Msg, State) ->
    case Msg of
        <<?CACHE_RAW_MESSAGE:8, BlockSize:16, Data/binary>> ->
            init_block_buffer(BlockSize, Data, Caller, State, raw_message);
        <<?CACHE_BLOCK_MESSAGE:8, BlockSize:16, Data/binary>> ->
            init_block_buffer(BlockSize, Data, Caller, State, block_message);
        <<?CACHE_MISSED_REFERENCE_INFO_MESSAGE:8, BlockSize:16, Data/binary>>  ->
            do_cache_missed_reference_info_message(BlockSize, Data, Caller, State);
        _ ->
            IsComplete = is_complete(State),
            if
                IsComplete ->
                    io:format("restart ~p \n", [Msg]),
                    {next_state, start, State, [{reply, Caller, ok}]};
                true ->
                    do_cached_missed_messages(Msg, Caller, State)
            end
    end.


raw_message({call, Caller}, get_data, State) ->
    get_data(Caller, State);
raw_message({call, Caller}, <<?CACHE_REFERENCE_MESSAGE:8, Key/binary>>, State) ->
    do_reference_message(Key, Caller, State);
raw_message({call, Caller}, Msg, State) ->
    do_message(Msg, Caller, State, raw_message).


block_message({call, Caller}, get_data, State) ->
    get_data(Caller, State);
block_message({call, Caller}, <<?CACHE_REFERENCE_MESSAGE:8, Key/binary>>, State) ->
    do_reference_message(Key, Caller, State);
block_message({call, Caller}, Msg, State) ->
    do_message(Msg, Caller, State, block_message, fun cache_store:save/1).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
                         {ok, NewState :: term(), NewData :: term()} |
                         (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
do_message(Data, Caller, State, NextState) ->
    do_message(Data, Caller, State, NextState, false).

do_message(Data, Caller, State, NextState, CallbackFun) ->
    Block = [Data | State#state.buffer#buffer.packets],
    BlockSize = size(binary:list_to_bin(Block)),
    ExpectedBlockSize = State#state.buffer#buffer.blocksize,
    io:format("BlockSize: ~p  Read so far: ~p\n", [ExpectedBlockSize, BlockSize]),
    if BlockSize == ExpectedBlockSize ->
            Block1 = binary:list_to_bin(lists:reverse(Block)),
            Stream = [Block1 | State#state.stream],
            %% call the callback function to persist data if we're in block state
            if
                is_function(CallbackFun) -> CallbackFun(Block1);
                true -> noop
            end,
            State2 = State#state{stream=Stream, buffer=#buffer{}},
            {next_state, start, State2, [{reply, Caller, ok}]};
       true ->
            Buffer = State#state.buffer#buffer{packets=Block},
            State1 = State#state{buffer=Buffer},
            {next_state, NextState, State1, [{reply, Caller, ok}]}
    end.

do_cached_missed_messages(Data, Caller, State) ->
    Block = [Data | State#state.buffer#buffer.packets],
    BlockSize = size(binary:list_to_bin(Block)),
    ExpectedBlockSize = State#state.buffer#buffer.blocksize,
    if BlockSize == ExpectedBlockSize ->
            Block1 = binary:list_to_bin(lists:reverse(Block)),
            Stream = [Block1 | State#state.stream],
            {ok, Key} = cache_store:save(Block1),
            List2 = lists:delete(Key, State#state.pending_refs),
            io:format("missed ref match ~p ~n", [Key]),
            State1 = State#state{stream=Stream, pending_refs=List2, buffer=#buffer{}},
            % same as: {keep_state,  State1, [{reply, Caller, ok}]}
            % make explicit to aid debugging
            {next_state, start, State1, [{reply, Caller, ok}]};
       true ->
            Buffer = State#state.buffer#buffer{packets=Block},
            State1 = State#state{buffer=Buffer},
            {next_state, start, State1, [{reply, Caller, ok}]}
    end.

do_reference_message(Key, Caller, State) ->
    case cache_store:lookup(Key) of
        {ok, Block} ->
            io:format("key found ~p ~n", [Key]),
            Stream = [Block | State#state.stream],
            RefOKMsg = cache_protocol_msgs:reference_ok_message(Key),
            {keep_state, State#state{stream=Stream}, [{reply, Caller, RefOKMsg}]};
        not_found ->
            io:format("key not found ~p ~n", [Key]),
            % add key to pending refs
            State1 = State#state{pending_refs=[Key|State#state.pending_refs]},
            MissedRefMsg = cache_protocol_msgs:missed_reference_message(Key),
            {keep_state, State1, [{reply, Caller, MissedRefMsg}]}
    end.

do_cache_missed_reference_info_message(BlockSize, Data, Caller, State) ->
    %% client is now sending packets for missing block
    init_block_buffer(BlockSize, Data, Caller, State, start).


init_block_buffer(BlockSize, Data, Caller, State, NextState) ->
    [] = State#state.buffer#buffer.packets,
    Block = [Data | State#state.buffer#buffer.packets],
    Buffer = #buffer{blocksize=BlockSize, packets=Block},
    {next_state, NextState, State#state{buffer=Buffer}, [{reply, Caller, ok}]}.

%% Handle events common to all states
get_data(Caller, State) ->
    Data = binary:list_to_bin(lists:reverse(State#state.stream)),
    %% if we're not waiting for blocks to be re-sent
    %% or additional packets to complete a block, return complete
    IsComplete = is_complete(State),
    Response =
        if IsComplete ->
                {complete, Data};
           true ->
                {incomplete, Data}
        end,
    %% Reply with the current stream
    {keep_state, State, [{reply, Caller, Response}]}.

is_complete(State) ->
    (State#state.pending_refs == []) and (State#state.buffer#buffer.packets == []).

process(Handler, Request) ->
    print_state_name(Handler),
    gen_statem:call(Handler, Request).

print_state_name(Handler) ->
    {CurrentState, _CurrentStateData} = sys:get_state(Handler),
    io:format("current_state: ~p ~n", [CurrentState]).
