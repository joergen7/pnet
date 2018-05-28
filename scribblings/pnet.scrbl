#lang scribble/manual
@require[@for-label[pnet
                    racket/place
                    racket/set
                    racket/contract
                    racket/bool
                    racket/base]]

@title{pnet: Petri nets on Racket places}
@author{Jörgen Brandt}

@defmodule[pnet]

Petri nets are an advanced transition system whose semantics are determined by
making explicit the preconditions enabling an operation to be carried out while
leaving implicit how independent operations are timed. This way Petri nets
provide a natural view on concurrent behavior.

This library allows the specification of the internals of a
@racketmodname[racket/place] as a Petri net, more specifically, a high-level
interface net. This means that (i) tokens can be any kind of Racket data
structure (with the only restriction that tokens that pass the Racket place
boundary, i.e., tokens sent and received by the net instance, need to be
@racket[place-message-allowed?]) and (ii) transitions can perform arbitrary
computations.

While a @racketmodname[pnet] instance acts as a Petri net on the inside, to the
outside it behaves like a normal Racket place. Receiving a message on a place
channel is translated to a token appearing or disappearing inside the Petri net.
Moreover, the net can be configured to trigger side effects like sending out
messages to other Racket places when a token appears in a particular position in
the net.

@section{API Functions}

This section describes all functions provided for controlling a Petri net
instance from the outside, i.e., functions to control an instance's life cycle
as well as functions to interact with an instance.

@subsection{Petri net life cycle}

Here, we discuss the functions provided for starting and stopping a Petri net
instance.

@defproc[(start-pnet-place [pnet-mod module-path?]
                           [init-arg place-message-allowed?] ...) place?]

This function starts a Petri net place instance. Like the @racket[place]
function it starts a place as a side effect and returns a reference to it. The
only mandatory argument @racket[pnet-mod] is a path to a Petri net module.

A Petri net module is a module, preferably in @racketmodname[typed/racket],
which provides a name @racket[*PNET*] being an instance of @racket[PnetPlace].
The Petri net module defines all application-dependent behavior of the place in
terms of a Petri net.

The remaining @racket[init-arg] arguments are processed by the
@racket[PnetPlace] struct's @racket[init] callback function which produces
the user info field used by the remaining callback functions.

@defproc[(stop-pnet-place [p place?]) void?]

@subsection{Interacting with Petri nets}

@defproc[(call [p place?] [msg place-message-allowed?]) place-message-allowed?]

bla

@defproc[(cast [p place?] [msg place-message-allowed?]) void?]

blub

@section{Structs}

@defstruct*[Delta ([consume hash?]
                   [produce hash?])]

This struct represents a change in the marking of a Petri net. It comprises a
consume-hash, specifying which tokens should be consumed from which places, and
a produce-hash, specifying which tokens should be produced on which places.

The struct is used to specify the effect of receiving calls or casts sent to the
net instance via @racket[call] or @racket[cast]. Accordingly, they are part of
the return values of the @racket[PnetPlace] callback functions
@racket[handle-call] and @racket[handle-cast].

@defstruct*[CallReply ([msg any]
                       [delta (or/c Delta false?)])]

This struct is the expected return value of the @racket[handle-call] callback
function and comprises a reply message to be returned by @racket[call] and an
optional @racket[Delta] struct representing the change in the Petri net marking.

@defstruct*[PnetPlace ([place-set    set?]
                       [preset-hash  hash?]
                       [init-marking (-> symbol? any list?)]
                       [enabled?     (-> symbol? hash? any boolean?)]
                       [fire         (-> symbol? hash? any (or/c hash? false?))]
                       [init         (->* () () #:rest (listof any) any)]
                       [handle-call  (-> any hash? any CallReply)]
                       [handle-cast  (-> any hash? any (or/c Delta false?))]
                       [trigger      (-> symbol? any hash? any boolean?)])]

This struct represents the application-specific definition of a Petri net. It
consists of the following fields:

@itemlist[
  @item{@racket[place-set] a set of symbols containing the names of all Petri
        net places.}
  @item{@racket[preset-hash] a hash table mapping symbols to lists of symbols
        and containing an entry for each transition mapping its name to the
        names of all places that are in its preset.}
  @item{@racket[init-marking] a function that, given a place name and the user
        info field generated via @racket[init], returns a list containing the
        tokens on that place when the net instance starts.}
  @item{@racket[enabled?] a predicate function that returns @racket[#t] if for a
        given trransition name, firing mode, and user info field enable that
        transition is enabled.}
  @item{@racket[init] a function that given a number of initial arguments
        returns the user info field}
  @item{@racket[handle-call] a function defining the reply and the changes in
        the Petri net marking as a consequence of a synchronous @racket[call].}
  @item{@racket[handle-cast] a function defining the changes in the Petri net
        marking as a consequence of an asynchronous @racket[cast].}
  @item{@racket[trigger] a function that given a place name, a token, and a
        user info field triggers a side effect and returns @racket[#t] if the
        token should be created normally on the place or @racket[#f] if the
        token should be dropped instead.}
]