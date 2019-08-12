-module(cache_sender).

-export([send_raw/1,
	 send_block/1,
	 send_key/1,
	 is_done/0,
	 stop/0,
	 start/0,
	 init/1,
	 terminate/2,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-include("cache.hrl").

-record(state, {socket, send_window = [], buffer = <<>>}).

send_raw(Data) when is_binary(Data) ->
    gen_server:cast(?MODULE, {send_raw, Data}).

send_block(Data) when is_binary(Data) ->
    gen_server:cast(?MODULE, {send_block, Data}),
    {ok, erlang:md5(Data)}.

send_key(Data) when is_binary(Data) ->
    Key = erlang:md5(Data),
    gen_server:cast(?MODULE, {send_key, Key, Data}).

is_done() ->
    gen_server:call(?MODULE, is_done).

stop() ->
    gen_server:call(?MODULE, stop).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 12000, [{active, false}, binary, {packet, 4}]),
    ok = inet:setopts(Socket, [{active, once}]),
    {ok, #state{socket = Socket}}.

terminate(_, _) ->
    ok.

handle_call(is_done, _From, #state{send_window = []} = State) ->
    {reply, true, State};
handle_call(is_done, _From, State) ->
    {reply, false, State};
handle_call(stop, _From, #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, normal, ok, State}.

handle_cast({send_raw, Data}, #state{socket = Socket} = State) ->
    ok = gen_tcp_send(Socket, cache_protocol_msgs:raw_message(Data)),
    {noreply, State};
handle_cast({send_block, Data}, #state{socket = Socket} = State) ->
    ok = gen_tcp_send(Socket, cache_protocol_msgs:block_message(Data)),
    {noreply, State};
handle_cast({send_key, Key, Data}, #state{socket = Socket, send_window = SendWindow} = State) ->
    ok = gen_tcp_send(Socket, cache_protocol_msgs:reference_message(Key)),
    {noreply, State#state{send_window = p_inc(Key, Data, SendWindow)}}.

handle_info({tcp, Socket, Data}, #state{socket = Socket,
					send_window = SendWindow,
					buffer = Buffer} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {NewBuffer, NewSendWindow} = reference_ack(Socket, <<Buffer/binary, Data/binary>>, SendWindow),
    {noreply, State#state{send_window = NewSendWindow, buffer = NewBuffer}};
handle_info(_, State) ->
    {noreply, State}.

p_inc(Key, Data, SendWindow) ->
    case lists:keytake(Key, 1, SendWindow) of
	false -> [{Key, 1, Data} | SendWindow];
	{value, {Key, Count, Data}, SendWindow2} -> [{Key, Count + 1, Data} | SendWindow2]
    end.

reference_ack(Socket, Buffer, SendWindow) ->
    case Buffer of
	<<?CACHE_MISSED_REFERENCE_MESSAGE:8, Key:16/binary, Rest/binary>> ->
	    {value, {Key, Count, Data}, SendWindow2} = lists:keytake(Key, 1, SendWindow),
	    ok = gen_tcp_send(Socket, cache_protocol_msgs:missed_reference_info_message(Data)),
	    NewSendWindow = p_dec(Key, Count, Data, SendWindow2),
	    reference_ack(Socket, Rest, NewSendWindow);
	<<?CACHE_REFERENCE_OK_MESSAGE:8, Key:16/binary, Rest/binary>> ->
	    {value, {Key, Count, Data}, SendWindow2} = lists:keytake(Key, 1, SendWindow),
	    NewSendWindow = p_dec(Key, Count, Data, SendWindow2),
	    reference_ack(Socket, Rest, NewSendWindow);
	NewBuffer ->
	    {NewBuffer, SendWindow}
    end.

p_dec(Key, Count, Data, SendWindow) ->
    case Count of
	1 -> SendWindow;
	_ -> [{Key, Count - 1, Data} | SendWindow]
    end.

gen_tcp_send(Socket, <<Data:256/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Data),
    gen_tcp_send(Socket, Rest);
gen_tcp_send(Socket, Data) ->
    gen_tcp:send(Socket, Data).
