#lang typed/racket/base

(require racket/set
         racket/match
         pnet)

(provide *PNET*)




(define place-set : (Setof Symbol)
  (set 'coin-slot 'cash-box 'signal 'storage 'compartment))

(define trsn-set : (Setof Symbol)
  (set 'a 'b))

(define preset-hash : (HashTable Symbol (Listof Symbol))
  (hash 'a '(coin-slot)
        'b '(signal storage)))

(: init-marking (Symbol Any -> (Listof Any)))
(define (init-marking place usr-info)
  (match place
    ['storage '(cookie-box cookie-box cookie-box)]
    [_        '()]))


(: enabled? (Symbol Mode Any -> Boolean))
(define (enabled? trsn mode usr-info)
  #t)

(: fire (Symbol Mode Any -> Mode))
(define (fire trsn mode usr-info)
  (match trsn
    ['a (hash 'signal '(sig))]
    ['b (hash 'compartment '(cookie-box))]))

(: init (Any * -> Any))
(define (init . arg-lst)
  'hello-world)

(: handle-call (Any Marking Any -> CallReply))
(define (handle-call msg marking usr-info)
  (match msg
    ['insert-coin       (CallReply 'ok (Delta (hash)
                                              (hash 'coin-slot '(coin))))]
    ['remove-cookie-box (if (null? (hash-ref marking 'compartment))
                            (CallReply #f #f)
                            (CallReply 'cookie-box
                                       (Delta (hash 'compartment '(cookie-box))
                                              (hash))))]))


(define *PNET* : PnetPlace
  (PnetPlace place-set
             trsn-set
             preset-hash
             init-marking
             enabled?
             fire
             init
             handle-call))