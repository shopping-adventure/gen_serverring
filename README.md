# GenServerring

A kind of distributed GenServer. Use as much as possible of the GenServer API
while distributing the state on a dynamic cluster/ring

GenServerring is an evolution of NanoRing. As NanoRing it uses a gossip protocol
to share the ring state and a payload without a master node. Both the ring (the
set of nodes) and the payload have to be CRDT. The ring is persistent and
written to disk. Managing the payload is the role of the client, GenServerring
is just sharing it amongst the nodes of the ring.

GenServerring is very similar to GenServer, so it provides the same API
(`init/1`, `call/2`, `cast/2`, `reply/2` and `stop/1`, `stop/2`, `stop/3`) and
expect the same callbacks (handle_call, handle_cast, handle_info).

On top of a classical GenServer, it provides 2 extra API calls (`add_node/2` and
`del_node/2`) to manage the ring. It also expects 2 extra callbacks
(`handle_ring_change` and `handle_state_change`) that are use to notify the
client in case of a change of the set of up nodes or payload respectively.

## How does it work? ##

Each node of the ring has a state consisting of shared elements
 - node_set: a CRDT OR-set representing all the nodes of the cluster
 - payload: a CRDT wich is managed by the client but shared by the ring

and a few elements that are kept locally
 - up_set: a MapSet keeping track of the nodes that are up. Each of the other
nodes of this set is monitored (Node.monitor/2)
 - counter: an increasing integer needed by Crdtex.update/3
 - callback: the module implementing the callbacks

### Sharing informations ###

The sharing of node_set and payload is done through a gossip protocol. Every
second, each node choose a random node amongst up_set and send it a tuple of
node_set, payload, and `node()`, let's call this tuple the gossip. Upon
reception of such a gossip, a node merge its own node_set and payload (using
`Crdtex.merge`) with the received one. In case of a change of payload or up_set,
the client is also notified through the corresponding callbacks.

### maintaining up_set ###

Each time a gossip is received from a node, we know that is node is up. If it
was not already in the up_set it is added there and the node is monitored.

Each time a node crashes a `:nodedown` message is received by each of the nodes
that were monitoring it. Upon reception of such a notice, up_set is updated.

This way, a node going down is triggering an immediate reaction on each of the
other nodes of the ring. A node joining the ring will be gradually discovered by
all the others through the gossip mechanism.

## testing ##

In order to test a cluster of 1 node (how exiting) use `./ct_test.sh`.

In ordor to test a cluster of 4 nodes wich should be more interesting use
`.//distributed_tests.sh`. This script expects the name of the common test
config file to use.

Both scripts use erlang common_test and place their results in `ct_logs` and
`ct_multi_logs` respectively

## usage ##
A cluster must have a local name (the same name amongst the node of the ring)
and a callback (it can varies amongst nodes but this is not advisable. I use
this possibility for testing purpose in `monitor_test.exs`).

Initially, each node is a ring containing only itself. You add an other node
with `GenServerring.add_node(ring_name, node)` (cf `writer_test.exs` for some
example). You can remove a node with `GenServerring.del_node/2`. You can check
which nodes are member of the ring (resp. up) with `GenServerring.all/1` (resp.
`GenServerring.up/1).

`demo.exs` shows an exemple of a client using `GenServerring`.
