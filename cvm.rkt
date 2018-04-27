;; -------------------------------------------------------------------
;; pnet
;;
;; Copyright 2018 Jörgen Brandt
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.#lang typed/racket/base
;;
;; -------------------------------------------------------------------

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
    ['a (hash 'signal '(sig) 'cash-box '(coin))]
    ['b (hash 'compartment '(cookie-box))]))

(: init (Any * -> Any))
(define (init . arg-lst)
  'hello-world)

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
    [_ (CallReply #f #f)]))


(define *PNET* : PnetPlace
  (PnetPlace place-set
             trsn-set
             preset-hash
             init-marking
             enabled?
             fire
             init
             handle-call))