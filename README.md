# pnet
###### Petri net library based on Racket places

[![Build Status](https://travis-ci.org/joergen7/pnet.svg?branch=master)](https://travis-ci.org/joergen7/pnet)

[Petri nets](https://en.wikipedia.org/wiki/Petri_net) are advanced transition systems whose semantics are determined by making explicit the preconditions enabling an operation to be carried out while leaving implicit how independent operations are timed. This way Petri nets provide a natural view on concurrent behavior.

This library allows the specification of the internals of a [Racket place](https://docs.racket-lang.org/reference/places.html) as a high-level interface net. This means that (i) tokens can be any kind of Racket data structure (with the restriction that tokens that pass the Racket place boundary, i.e., tokens sent and received by the net instance, need to be [place-message-allowed?](https://docs.racket-lang.org/reference/places.html#%28def._%28%28lib._racket%2Fplace..rkt%29._place-message-allowed~3f%29%29)) and (ii) transitions can perform arbitrary computations.

While a pnet instance acts as a Petri net on the inside, to the outside it behaves like a normal Racket place. Receiving a message on a place channel is translated to a token appearing or disappearing inside the Petri net. Moreover, the net can be configured to trigger side effects like sending out messages to other Racket places when a token appears in a particular position in the net.

## Usage

The Petri net is specified by constructing a typed struct. The struct must be populated by both data and callback functions to determine the structure of the net as well as the behavior of the net as a Racket place. Then, this struct needs to be exported by a module so that it can be dynamically loaded upon starting the Petri net instance.

Suppose, we have already created such a module and stored it in the module path `"git/pnet/cvm.rkt"`. We can start it by using the function `start-pnet-place` exported by the pnet library.

```racket
#lang racket

(require pnet)

(define p
  (start-pnet-place "git/pnet/cvm.rkt"))
```

The net can be interacted with using the functions `call` (for synchronous message exchange) and `cast` (for sending a message asynchronously) to send it messages like `'insert-coin` or `'remove-cookie-box`.

```racket
(call p 'insert-coin)
(call p 'remove-cookie-box)
```

The net can be stopped by using the function `stop-pnet-place`.

```racket
(stop-pnet-place p)
```

## Example: Cookie Vending Machine

![Cookie Vending Machine](priv/cvm.png)

*Figure 1: Petri net model of a cookie vending machine. The machine stores three cookie boxes in its storage. Upon inserting a coin, it relays a signal to release one of its cookie boxes into the compartment from where it can be removed.*

Consider a cookie vending machine as specified in Figure 1. The net consists of two transitions named `a` and `b` and five places named `coin-slot`, `cash-box`, `signal`, `storage`, and `compartment` of which `storage` holds three identical tokens `cookie-box`. In the following, we explain how to construct the elements of a `PnetPlace` struct using the cookie vending machine as a running example. We do so in typed Racket, so the module starts by defining the language and requiring the `pnet` package and providing the struct defined in `*PNET*`.

```racket
#lang typed/racket/base
(require pnet)
(provide *PNET*)
```

### place-set

The first field of the `PnetPlace` structure exported by the `pnet` package is `place-set`. This is a symbol set enumerating the names of all places in the Petri net.

```racket
(define place-set : (Setof Symbol)
  (set 'coin-slot 'cash-box 'signal 'storage 'compartment))
```

### preset-hash

For each transition in the net, we need a key-value pair in the field `preset-hash` where the key is a symbol denoting the name of the transition and the value is a list of symbols referring to the names of all places appearing in the transition's preset.

```racket
(define preset-hash : (HashTable Symbol (Listof Symbol))
  (hash 'a '(coin-slot)
        'b '(signal storage)))
```

### init-marking

The function `init-marking` takes a place name and a user-defined data item and returns a list containing the tokens constituting the initial marking of the Petri net.

```racket
(: init-marking (Symbol Any -> (Listof Any)))
(define (init-marking place usr-info)
  (match place
    ['storage '(cookie-box cookie-box cookie-box)]
    [_        '()]))
```

### enabled?

The function `enabled` takes a transition name and a firing mode and determines whether the firing mode enables this transition.

```racket
(: enabled? (Symbol Mode Any -> Boolean))
(define (enabled? trsn mode usr-info)
  #t)
```

### fire

The function `fire` takes a transition name and a mode that enables this transition and determines what tokens firing this transition produces on what places.

```racket
(: fire (Symbol Mode Any -> Mode))
(define (fire trsn mode usr-info)
  (match trsn
    ['a (hash 'signal '(sig) 'cash-box '(coin))]
    ['b (hash 'compartment '(cookie-box))]))
```

### init

The function init is called at the beginning of the net instance's life cycle and constructs the user info field which is passed to many of the callback functions.

```racket
(: init (Any * -> Any))
(define (init . arg-lst)
  #f)
```

### handle-call

From the outside, the net instance can be addressed synchronously by calling it with the function `call`. When a call is received by the net instance, it uses the callback function `handle-call` to determine the call reply.

```racket
(: handle-call (Any Marking Any -> CallReply))
(define (handle-call msg marking usr-info)
  (match msg
    ['insert-coin       (CallReply (void) (Delta (hash)
                                              (hash 'coin-slot '(coin))))]
    ['remove-cookie-box (if (null? (hash-ref marking 'compartment))
                            (CallReply '() #f)
                            (CallReply '(cookie-box)
                                       (Delta (hash 'compartment '(cookie-box))
                                              (hash))))]
    [_ (CallReply 'bad-msg #f)]))
```

### handle-cast

The net instance can be addressed asynchronously by casting a message with the function `cast`. When a casted message is received by the net instance, it uses the callback function `handle-cast` to determine the reply.

```handle-cast
(: handle-cast (Any Marking Any -> (U Delta False)))
(define (handle-cast msg marking usr-info)
  #f)
```

### trigger

Whenever a token appears on a place we have the possibility to associate it with a side-effect via the function `trigger`. In addition we can choose whether the token should vanish (by returning `#f`) or whether the token should actually appear (by returning `#t`).

```racket
(: trigger (Symbol Any Marking Any -> Boolean))
(define (trigger trsn token marking usr-info)
  #t)
```

### Constructing the Petri Net Struct

Now that we defined all fields of the Petri net struct individually, we can construct it using the function `PnetPlace`.

```racket
(define *PNET* : PnetPlace
  (PnetPlace place-set
             preset-hash
             init-marking
             enabled?
             fire
             init
             handle-call
             handle-cast
             trigger))
```

## System Requirements

- [Racket](http://racket-lang.org/) 6.10 or higher

## Resources

- [joergen7/gen_pnet](https://github.com/joergen7/gen_pnet) A generic Petri net OTP behavior for Erlang.


## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)


