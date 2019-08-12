An erlang process that receives encoded cache messages on a tcp connection and reconstructs a chunk of binary data.

## How it works

The data is sent in small packets wrapped up in a protocol that directs the receiver to:

- just put the data into the reconstructed chunk
- put the received block of data into the reconstructed chunk and save the block of data for future reference
- retrieve a reference and put the block into the reconstructed chunk
  - if the block for the reference is found the receiver must reply to the sender with an 'ok'
  - if it's not found the receiver must reply to the sender with a 'missed reference'

- send_raw is an instruction to insert the given data into the data stream and then forget about it,
- send_block is an instruction to insert the given data into the data stream and then save it, as it might be referred to by ID later on.

For each 'missed reference' message that the receiver sent it should expect a reply with the missing info.

These protocol messages are constructed in: cache_protocol_msgs.erl. And defined in: cache.hrl

### Implementation

Network Cache application implemented with a state machine: gen_statem

Once a client is connected, the TCP server passes the connection to a state machine. The state machine has 3 states: start, raw_message, block_message. It begins in the start state and evolves based on the commands/data received from the client. It’s designed such that regardless of the state we are in, we are able to process get_data messages.

* while in block/raw message, expect more packets and add to temp store of packets
    * check if existing packets add up expected block size
    *   If block size match,
    *  save to store and enter read state, mark buffer as complete and save block to the store IF in block state
    * if current size of data in buffer deosn't match blocksize, remain in state, incomplete

* if message is a key reference:
    * if exists in store, add to stream
    * else send missing reference message back
        * if client responds with missing info, enter block state. Add data to the store and the stream

* if get_data
    * if current read buffer is complete, i,e all the packets received
        * then return {complete, data stream
    * else return {incomplete, data_stream}



## Cache receiver API

The cache_receiver.erl module uses cache_store.erl to store any blocks it needs.

On startup it calls cache_store:init(). And to store / retrieve it uses:

* cache_store:save(Block). % which returns {ok,Key}

* cache_store:lookup(Key). % which returns {ok,Block} or not_found

### More on cache_receiver API:

* get_data/0 - returns either {complete, Data} or {incomplete, Data},
             depending on whether we're waiting for blocks to be re-sent.
* start/0 - start the process with a registered name
* stop/0 - end the registered process

The process listens on port 12000 for the sender process to connect and send data.
