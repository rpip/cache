An erlang process that receives encoded cache messages on a tcp connection and reconstructs a chunk of binary data.

## How it works

The data is sent in small packets wrapped up in a protocol that directs the receiver to:

- just put the data into the reconstructed chunk
- put the received block of data into the reconstructed chunk and save the block of data for future reference
- retrieve a reference and put the block into the reconstructed chunk
  - if the block for the reference is found the receiver must reply to the sender with an 'ok'
  - if it's not found the receiver must reply to the sender with a 'missed reference'

For each 'missed reference' message that the receiver sent it should expect a reply with the missing info.

These protocol messages are constructed in: cache_protocol_msgs.erl. And defined in: cache.hrl

## Cache receiver API

The cache_receiver.erl module uses cache_store.erl to store any blocks it needs. On startup it calls cache_store:init(). And to store / retrieve it uses:

* cache_store:save(Block). % which returns {ok,Key}
** cache_store:lookup(Key). % which returns {ok,Block} or not_found

### More on cache_receiver API:

* get_data/0 - returns either {complete, Data} or {incomplete, Data},
             depending on whether we're waiting for blocks to be re-sent.
* start/0 - start the process with a registered name
* stop/0 - end the registered process

The process listens on port 12000 for the sender process to connect and send data.
