%%
%% The on the wire XDR dataplance protocol.
%%

-module(cache_protocol_msgs).

-export([
	 raw_message/1,
	 block_message/1,
	 reference_message/1,
	 reference_ok_message/1,
	 missed_reference_message/1,
	 missed_reference_info_message/1
	]).

-include("cache.hrl").

%%
%% Generates a raw message containing the specified data.
%%
raw_message(Data)  ->
    BlockSize = size(Data),
    <<?CACHE_RAW_MESSAGE:8, BlockSize:16, Data/binary>>.

%%
%% Generates a raw message containing the specified data 
%%      with the given block id.
%%
block_message(Data) ->
    BlockSize = size(Data),
    <<?CACHE_BLOCK_MESSAGE:8, BlockSize:16, Data/binary>>.

%%
%% Generates a reference message using the given id.
%%
reference_message(Key) ->
    16 = size(Key),
    <<?CACHE_REFERENCE_MESSAGE:8, Key/binary>>.

%%
%% Generates a reference ok message using the given id.
%%
reference_ok_message(Key) ->
    16 = size(Key),
    <<?CACHE_REFERENCE_OK_MESSAGE:8, Key/binary>>.

%%
%% Generates a missed reference message using the given id.
%%
missed_reference_message(Key) ->
    16 = size(Key),
    <<?CACHE_MISSED_REFERENCE_MESSAGE:8, Key/binary>>.

%%
%% Sent in response to a cache miss message. Provides the missing block.
%%
missed_reference_info_message(Data) ->
    BlockSize = size(Data),
    <<?CACHE_MISSED_REFERENCE_INFO_MESSAGE:8, BlockSize:16, Data/binary>>.
