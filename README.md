# pnet
###### Petri net library based on Racket places.

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



## System Requirements

- [Racket](http://racket-lang.org/) 6.10 or higher

## Resources

- [joergen7/gen_pnet](https://github.com/joergen7/gen_pnet) A generic Petri net OTP behavior for Erlang.


## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)


