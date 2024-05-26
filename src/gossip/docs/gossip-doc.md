**Gossip.md**

20-May-2024

 

In which I try to collect documentation for my gossip system, how it works, and
how to use it. (My gossip system is in
`#P"~/Lisp/third-party/emotiq-archive/gossip"`. I wrote it while I was working
for Emotiq but it's open-source software under an MIT license. I should probably
break it out at some point, but for right now it will load by

`(ql:quickload :gossip)`

as long as the emotiq-archive folder is known to ASDF.

 

How gossip works. (I'm writing this to refresh my own memory, since it's been 6
years

since I wrote the gossip code.)

 

Introduction
------------

This code is an implementation of *gossip protocol*. Here's the problem it
solves: What if you have a large group of nodes (and nodes can be anything --
hardware or pure software) that need to establish some kind of connectivity to
each other and knowledge of each other and then communicate messages about the
mission domain with each other? And (this is key!) you want them to
self-organize this network because there's no godlike master control node
setting everything up.

Furthermore, it is probably the case that you will never be able to have direct
communications between every node `n_i` and `n_j`. For that to be possible the
number of connections grows as N\^2, which generally doesn't work well in the
real world. Gossip enables messages to be passed along from node-to-node as
needed until the message arrives at its destination, without requiring N\^2
direct links. Thus it's a mesh network. (More properly, gossip is a protocol
that works well for sending messages across mesh networks, especially when you
want to send the same message to all the nodes.) Gossip is reliable in the face
of node and link outages, and of new nodes dynamically becoming available.

 

<https://en.wikipedia.org/wiki/Gossip_protocol>

 

 

Note & Caveats
--------------

This document doesn't [[yet]] talk much about sending gossip messages across a
network. We'll get to that later.

 

This gossip code assumes an **explicit** graph. That is, it assumes every node
is made aware of its direct neighbors *a priori* via its `neighbors` slot. This
is a reasonable assumption for a wired network like the Internet. But in an
unwired (implicit) network like a bunch of nodes communicating via
omnidirectional RF or light or ultrasound, it's not a valid assumption. In such
a network there needs to be some model of a signal being sent omnidirectionally
as well as a model of a propagation medium that adds noise, multipath
distortion, dispersion, etc. and also dissipates the signal with distance.

The current gossip code provides no such models.

 

Documenting Gossip from its Docstrings
--------------------------------------

`(ql:quickload :cl-simpledoc)`

`(with-open-file (s "~/Lisp/svs-repos/gossip/gossip.html" :direction :output
:if-exists :supersede)`

`(cl-simpledoc:print-package-docs :gossip s :external t :internal t :variables t
:functions t :macros t :classes t :generic-functions t))`

 

Simple-Gossip.lisp (and .asd)
-----------------------------

`#P"Simple-gossip.lisp"` does not use actors. This appears to be an early
attempt to test gossip protocol. No changes to this file were made later than
April 2018, so it should probably be considered obsolete, except as a record of
how I approached the problem initially. Much of its API carried forth into the
actor-based `#P"gossip.lisp"`.

The Overseer vs. The Nodes
--------------------------

Gossip is both a network of actor-based software nodes, and a simulation
environment for simulating such networks. We're going to call the human who sets
up a gossip network and runs a simulation the **overseer** herein. The overseer
is omniscient; gossip nodes are not -- although to have gossip nodes figure out
things that the overseer knows is often the whole point of a gossip protocol.

The *gossip realm* is that which is known by the nodes.

The *overseer realm* completely subsumes the gossip realm and includes
everything the Lisp process knows.

Various constants, global variables, and even functions in this code base are
intended to be known only by the overseer, and should not be considered
available for inspection by nodes. This is the **overseer rule**. One example of
this is the `*nodes*` table which keeps track of all nodes known to this Lisp
process. Nodes themselves typically only have a priori knowledge of their
immediate neighbors -- and sometimes they don't even know that much. Sometimes
part of the mission of a gossip network is for each node to build up a picture
of the entire network and ideally for all the nodes to agree on that picture.

 

Call \#'gossip-init once before you start using the gossip system. This

sets up the logging mechanism.

See \#P"gossip-test.lisp" for more info.

 

Every gossip-node contains its own actor of class 'gossip-actor.

In a sense this actor \_is\_ the node.

When a gossip-node is made an actor is also made for it.

(See \#'initialize-node.)

 

A gossip-node knows its actor (via its actor slot)

and that actor knows which gossip-node it belongs to (via its node slot).

 

 

\#'make-proxy-node makes a proxy-node.

 

Overseer API
------------

`#'listify-nodes` returns a list of all nodes in the `*nodes*` table, i.e. all
nodes known to this machine, including temporary nodes.

`#'lookup-node` looks up a node in the `*nodes*` table by its UID.

`#'locate-local-uid-for-graph` returns some local node for the given graphID.
Useful when you want to send a message to the graph and you don't care at which
node the message starts.

`#'singlecast` sends a message intended for a specific nodeID via the gossip
network. Message can be started at some specific nodeID or one will be selected
at random.

 

\#'broadcast

\#'gossip-dissolve-graph

 

REAL Nodes
----------

A 'real' node is of class `gossip-node` and it's not temporary. Because
`proxy-gossip-node` does not inherit from `gossip-node` [probably should name it
something else because its name suggests otherwise], a real node is not a
`proxy-gossip-node` either.

 

### API

`#'make-node` makes a gossip-node

`#'memoize-node` adds the node to the global `*nodes*` table keyed on its UID.

`#'real-node-p` returns true if a node is real.

`#'local-real-nodes` returns a list of nodes that are real (non-proxy),
non-temporary and resident on this machine.

 

 

PROXY Nodes
-----------

A proxy node is one that stands in for a real node on another machine.
Proxy-nodes are useful for network communication: Every node you want to talk to
on a remote machine will have a proxy node on *this* machine. Messages get sent
to remote nodes by sending them to the appropriate proxy-node on this machine.
Proxy nodes are of class `proxy-gossip-node` and they do *not* inherit from
`gossip-node`.

Proxy nodes are always memoized in the `*nodes*` table on this machine.

API:

`#'make-proxy-node` makes a proxy-node.

 

TEMPORARY Nodes
---------------

A node is temporary if its `temporary-p` slot is true. Anything that inherits
from `abstract-gossip-node` gets this slot, and that includes `gossip-node` and
`proxy-gossip-node`. Temporary nodes are generally not findable in the local
node table. Temporary nodes *do* get memoized in `*nodes*`, but most lookup
functions ignore them because of their `temporary-p` slot value.

What's the point of temporary nodes? I think the main answer is that sometimes
they're useful. If the outside world wants to send a message to the graph, it's
useful to make a temporary node, set its `neighborhood` to be a few nodes in the
graph, and then send a message to it. [do we need more here?]

Temporary nodes should probably be renamed 'infrastructure' or 'overseer' nodes
because [[I think]] they are only used for infrastructure purposes, not domain
purposes. In other words they are part of the simulation or test harness
environment rather than the simulat**ed** environment.

 

Node sets
---------

`*nodes*` is the master table of nodes known by the local machine. This includes
temporary nodes. The nodes themselves don't know about `*nodes*`; it exists
strictly for the overseer. In gossip this is just a global variable. In David
McClain's actors code (upon which gossip rests), he seems to be using an actor
for this purpose. See `#'ac:register-actor`. Thus while I'm merely using actors
for the gossip realm, David is using them in the overseer realm as well. Turtles
all the way down.

Of course I'm using an actor for logging. See `*logging-actor*.`

 

`+default-graphid+` is the identifier of the default, root, ground, or
'physical' graph that a node is always part of. Looks like this is mostly used
in `graphviz.lisp`.

`graphID` (the parameter for which `+default-graphid+` is the default value
which is literally `:root`) is a slot of a gossip message. This parameter
determines which graph the current message should propagate through. Since nodes
can be part of multiple graphs, this identifies which set of neighbors to use
when forwarding the message. `+default-graphid+` will be the default ground
graph which can be considered a 'physical' graph, while the rest are 'logical'
but this is just a convention. `#'get-downstream` pays attention to `graphID`
when choosing downstream neighbors. Any given node's `neighbors` table contains
a list of its neighbors indexed per `graphID`; this is the mechanism that allows
a node to be part of multiple graphs.

 

Gossip messages fall into 3 classes:

1.  Solicitations

2.  Replies to solicitations

3.  System-async

 

"Upstream" means "back to the solicitor: the node that sent me a solicitation in
the first place."

"Downstream" means the normal direction of solicitation messages through the
gossip network.

 

From an actor's point of view, all gossip messages are of type :gossip. Actors

don't distinguish between Solicitations and Replies.

 

What messages (solicitations) can we send?

:count-alive

:list-alive

:find-max

:find-min

:gossip-relate

:gossip-lookup-key

 

Logging
-------

The way we can watch what's going on is through `*log*`. `*logging-actor*` is
itself

an actor. Its default operation is `'actors:do-nothing`. If you want logging to
actually work, do this:

`(setf *logging-actor* (ac:make-actor 'actor-logger-fn))`

This is what `#'gossip-init` does. This will make every message sent to a
gossip-node record a log entry in `*log*`.

 

API:

`#'log-event` Sends timestamp and args to `*logging-actor*`.

`#'default-logging-function` Default logger for gossip nodes. Filters using the
`*log-filter*` mechanism and then calls `#'log-event`.

`*log-filter*` t, nil or a predicate that determines whether to log a given
gossip message.

`#'log-exclude`, `#'log-include` Return predicates that can be assigned to
`*log-filter*`.

`#'edebug` Adds a message to the log if its first parameter is \<
`*debug-level*` or if `*debug-level*` is t. Second parameter can be a string or
a gossip-node, in which case that node's logfn (which is usually
`#'default-logging-function`) is called. Filtering only applies here in the
second case.

 

Hemlock Logging
---------------

It's possible to send log messages to a Hemlock window in the CCL IDE, but it
appears that I didn't use this much (if at all) for gossip nodes. Probably
because it becomes terribly slow as the content of the window gets large. See
`#P"hemlock-log-streams.lisp"`.

 

Initialization
--------------

`#'gossip-init` seems to be the common function that every kind of startup
mechanism must call.

`#'gossip-startup` seems to be for setting up a true network of nodes with
multiple machines communicating with TCP/IP by reading configuration files.

The file `#P"gossip-test.lisp"` and the others in its folder seem the best
candidates for setting up a gossip network on a single machine.

`#'run-gossip` seems to be what you want if you want to run a network-based
simulation.

`#'make-node` makes one gossip-node on the local machine, sets up an actor for
it, and sticks it in the `*nodes*` table. It does not connect the node to any
other nodes.

`#'make-nodes` just calls `#'make-node` n times.

`#'make-graph` calls `#'make-nodes` and then connects them into a graph. The
graph is guaranteed to be a single graph (i.e. no node or subset of nodes is
isolated) but it will not be fully connected. By default the total number of
connections will be about 1.5\*N rather than 0.5\*N\^2 for a fully connected
graph.

 

Sending Messages
----------------

At core, sending a message means calling `#'actor-send` which adds a message
onto an actor's incoming mailbox. Said mailbox contains a priority queue. [[I'm
a little fuzzy on the difference between`#'mailbox-send` and
`#'deposit-message`, except that the former is protected with a semaphore. This
is David McClain's code and I don't remember the details. I also don't remember
what a 'stashed' message means to an actor.]]

At a slightly higher level `#'send-msg` figures out the destination actor owned
by the destination node and then calls `#'actor-send` to send an
actor-compatible message whose actor-verb is `:gossip` followed by the UID of
the source followed by the gossip message. As a special case, `#'send-msg` with
a destuid of 0 broadcasts it to all real nodes in the `*nodes*` database. This
is intended to be used by incoming-message-handler methods for bootstrapping
messages before `#'neighbors` connectivity has been established. [[Not sure what
last 2 sentences mean, except they violate the overseer rule. But it may be the
case that this mechanism is only used by the overseer anyway.]]

 

Percolation
-----------

How do messages percolate through the network? Two slots in every message
control how this works: `forward-to`and `kind`.

The `forward-to` slot determines how many downstream nodes of the current node
will have the current message forwarded to them. Allowed values:

-   t, which means ":neighborcast" which means all the node's neighbors (except
    the one the message came in on) get the message forwarded to them.

-   nil, which means never forward the incoming message.

-   n (an integer) which means to randomly choose up to n of the node's
    neighbors and forward the message to them.

The `kind` slot which determines whether the current node does anything *other
than* forwarding the message. In other words, does the current node process the
message locally somehow before forwarding it. Allowed values:

-   `:k-singlecast` If the destination of this message is in fact this node,
    call `#'handoff-to-application-handlers` and do not forward. Otherwise just
    forward according to `'forward-to`. No reply expected.

-   `:k-multicast` Always call `#'handoff-to-application-handlers` and *also*
    forward according to `'forward-to`. No reply expected.

-   `:k-hello` Mostly for use across IP networks. [[more here]]

-   `:k-dissolve` Don't call `#'handoff-to-application-handlers`. Forward the
    message to all neighbors (i.e. ignore `'forward-to`), and then isolate this
    node within this message's graphID. In other words destroy this node's
    knowledge of its neighbors. No reply expected.

-   `:gossip-relate` Establishes a non-unique key/value pair on this node. Sets
    value in this node and then forwards according to `'forward-to`. No reply
    expected.

-   `:gossip-relate-unique` Like `:gossip-relate` but if a previous value for
    given key exists in node, replace it with new value. No reply expected.
    **This operation is destructive***.*

-   `:gossip-remove-key` Removes key/value pair on this node and then forwards
    according to `'forward-to`. **This is a destructive operation** -- any node
    that currently has the given key will have that key/value removed. There's
    no harm in calling this more than once with the same key; if key wasn't
    present in the first place, this is a no-op. No reply expected.

-   `:gossip-tally` Increment the value of a given key by an increment amount.
    If no value for that key exists currently, set it to 1. No reply expected.

-   `:timeout` Timeouts are a special kind of message in the gossip protocol,
    and they're typically sent by a special timer thread.

[[more kinds deal with replies. Document them later.]]

 

Replies
-------

[[I don't remember how replies work and I don't need them at present. Document
them later.]]

 

All the above kinds occur as both keywords in the 'kind slot of messages, and as
functions or methods in their own right. **Those functions and methods decide
the ultimate fate of the message.** In general, each 'kind' function can check
to see if the message was directed to the node running the method, in which case
it calls `#'handoff-to-application-handlers`, or if not, it simply forwards the
message. *How* it forwards the message is determined by calling
`#'get-downstream` on this node, the `forward-to` slot of the message, and the
`graphID` of the message.

Note that some of the above are useful to domain programmers in their own right,
like `#'gossip-relate` and `#'gossip-relate-unique`. Domain programmers with
more specialized needs for local message handling should specialize
`#'application-handler`.

 

Destinations
------------

The concept of the "destination" of a message can be confusing. \#'send-msg has
a second parameter of the nodeID where the message will be sent. But that's
merely the next node in a chain. If the message is actually *intended* for a
specific destination, that destination nodeID is included in the message object,
as the first value in the list in the args slot. That's why you see` (car (args
msg))` in `#'k-singlecast`.

 

Timeouts
--------

 

`*master-timer*` agent sends `system-async` msg with `kind=:timeout` to
gossip-node. Node looks up `solicitation-uid` of msg in its `timeout-handlers`.
If it finds one, it calls it with an arg of t (because it timed out). This
implies that the `*master-timer*` has to send a :timeout message with that exact
`solicitation-uid`. Timeout-handler typically comes from
`#'make-timeout-handler`, which returns a lambda that deals with timeouts.
Typically to clean up stuff that was waiting on a reply.

 

 

[[edit the following crap]]

To send a message that doesn't require a reply:

Use \#'solicit. (Although \#'solicit is not intended for use by an node
messaging another node.)

To send a message that does require a reply:

Use \#'solicit-wait.

\#'broadcast is more modern than \#'solicit.

 

To send a message that you want a direct response to:

Use \#'solicit-direct and make your message manually with the :reply-to slot
being the UID of the node that should expect all replies.

This tells recipient node to reply to you directly.

 

To send a message from a NODE that the node wants a direct response to:

The node should make the message manually with the :reply-to slot being the UID
of the node itself.

The node should call (send-msg msg uid \#'final-continuation), where uid is the
UID of the node itself [yes, you send

the message to yourself] and final-continuation is a single-argument function of
a reply, where that reply is the coalesced reply

the node itself would send upstream if it were an :UPSTREAM message. If
final-continuation is nil, that reply just gets

dropped on the floor.

 

Direct replies are weird, and they're intended to be. Grep for string "direct
messages can be weird".

 

 

Kinds of protocols in general: (\#"get-downstream decides where to forward a
message to once a node receives it)

:gossip style to pick one neighbor at random to forward messages to.

There's no guarantee this will reach all nodes. But it's quicker and more
realistic. [[SVS: Than what?]]

Specify by setting forward-to slot of your solicitation message to an integer n,
meaning

to forward the message to up to n of your immediate neighbors (never including
the upstream node this node got it from of course).

 

:neighborcast style to send solicitations to all neighbors of originator (never
including the upstream node this node got it from of course).

More likely to reach all nodes but replies may be slower. [[SVS: I don't
understand this comment. Slower than what? :gossip? A direct reply?]]

Specify by setting forward-to slot of your solicitation message to anything
that's true but not an integer.

 

Neither of above styles should result in timeouts of a node waiting forever for
a response from a neighbor.

 

 

Kinds of replies:

:UPSTREAM -- Much like returning the result from a function. Uses the
coalescence mechanism.

:GOSSIP -- Requests reply via a new message that gets gossiped back to
originator.

:NEIGHBORCAST --

 

Everything having to do with "transport" seems to be about operation over

a network. Likewise proxy nodes.

"The GOSSIP/TRANSPORT API delivers messages between Gossip nodes

asynchronously over the network with best-effort ("send and pray")

semantics."

 

[[end of crap]]

What Does a Message Look Like?
------------------------------

The message the actors see is (:gossip srcuid \#\<gossip-message-mixin\>). It's
just the gossip-message prepended with the UID of the source gossip-node and the
keyword :gossip. See gossip-message-mixin...

When messages are sent over the network, they are 5 pieces in a list:

-   destuid of ultimate receiving node

-   srcuid of sending node

-   remote-address (eripa) of sender

-   remote-port of sender which is usually `*actual-tcp-gossip-port*`

-   gossip-msg

Network messages are serialized with Sean Ross' `cl-store` library.

 

Receiving Messages
------------------

How does a gossip-node receive a message? The toplevel function is
\#'gossip-dispatcher, which is where the receiving process begins. The actor
attached to the node runs this function whenever it sees a new message on its
queue. [[actually the dispatch function is a lambda wrapped around
`#'gossip-dispatcher`, and for clarity that should probably be replaced by an
:around method on `#'gossip-dispatcher.`]]

`#'gossip-dispatcher` makes sure the first element of the message is `:gossip`,
checks that there is indeed a gossip-node that contains this actor, extracts the
`srcuid` which should be the second element of the original message, and calls
`#'deliver-gossip-msg` on the third element of the original message.

*In other words* the message the actor sees is` (:gossip srcuid
#<gossip-message-mixin>)`. It's just the gossip-message prepended with the UID
of the source gossip-node and the keyword :gossip.

`#'deliver-gossip-msg` expects to see the true gossip-message as its first
parameter, together with the gossip-node and srcuid. It copies the message,
increases its hopcount, and then calls `#'locally-receive-msg`. The reason the
message is copied is that we have to increase its hopcount before forwarding it,
and if we did not copy it first, other nodes on this machine that are currently
processing the message would magically see its hopcount increase. [[we should
probably improve this to make messages in transit be a tuple of (hopcount .
\<original-message\>) where \<original-message\> is treated as immutable. This
would reduce the consing that messages in transit now incur.]]

`#'deliver-gossip-msg` also takes care of sending messages across the network in
the case of proxy-gossip-nodes.

(There's a function called `#'incoming-message-handler`which is strictly for
handling messages that come in across the network from another machine. This
function routes such messages to the proper node on the local machine.)

`#'locally-receive-msg` is where stuff starts to get real. This function checks
whether this node should be accepted at all by this node, and if so it memoizes
it (so it can tell if it ever receives it a second time) and calls
`#'locally-dispatch-msg`. The two main reasons why an acceptance fails are:

-   This node has seen the same message before

-   The message was an :active-ignore reply, which is only used to reply to
    :UPSTREAM messages. Whomever sent it is telling us they're ignoring us.
    Which means we need to ensure we're not waiting on them to reply. [[not
    completely sure what this means]]

`#'locally-dispatch-msg` then funcalls the symbol in the `kind` slot of the
message (after reinterning its symbol-name in the `:gossip` package and checking
that it's fboundp there). See **Percolation** section for more information about
the `kind` slot.

There are two mechanisms of application-handlers: `#'application-handler` and
`#'lowlevel-application-handler`. There are two mechanisms because:

1.  `#'lowlevel-application-handler` is really intended only for infrastructure
    use, and it probably shouldn't be specialized by application programmers. By
    default it looks for a function in \*`ll-application-handler`\* and if that
    itself is nil, it returns a function that just logs the fact that the
    lowlevel-application-handler was reached with the current message. I think
    the main use of `*ll-application-handler*` is that you can stick a function
    in there for debugging and testing by the overseer, and you can override the
    method to return nil if you want to disable all `#'application-handler`s.

2.  `#'application-handler` is only called if `#'lowlevel-application-handler`
    returns non-nil. So if you specialize or override
    `#'lowlevel-application-handler` for a particular node type and make it
    return nil, then any `#'application-handler` for that type of node will
    never be called.

Again, the application-handlers are only called when a node receives a message
and *something in addition to forwarding the message* is required of the node.
Thus the application-handlers are only called if the node decides it needs to
read and act on the message locally. Sometimes it does so *and* forwards the
message (e.g. in the case of `:k-multicast`) and sometimes it does the first XOR
the second (e.g. `:k-singlecast`).

You can specialize `#'application-handler` for particular types of nodes. The
default method returns a function that merely logs the fact that
`application-handler` was reached with the current message. [[It is interesting
that I cannot find any cases in the emotiq code base where
`#'application-handler` was specialized. Message kinds like `:gossip-relate` and
`:gossip-relate-unique` might have been all we needed as domain programmers.]]

 

 

Forwarding Messages
-------------------

See **Percolation** section.

 

Crypto
------

Any time you see "pkey" or "skey" those mean "public key" and "secret key".

 

Saving A Gossip Graph
---------------------

`#'save-graph` will print the graph of (local-real-nodes) to a stream as Lisp
source code.

Glossary
--------

eripa: Externally-Routable IP Address of this machine. `#'eripa` looks this up
via `#'http-fetch`.
