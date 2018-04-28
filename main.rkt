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

#lang racket/base

;;====================================================================
;; Requirements
;;====================================================================

(require racket/place
         racket/contract
         racket/match
         racket/bool
         racket/set
         "lib-pnet.rkt")

;;====================================================================
;; Provisions
;;====================================================================

(provide ; Type Definitions
         Mode
         Marking

         ; Struct Definitions
         (struct-out PnetPlace)
         (struct-out Delta)
         (struct-out CallReply)

         ; API Functions
         (contract-out
          [start-pnet-place ((module-path?) () #:rest any/c . ->* . place?)]
          [stop-pnet-place  (place? . -> . void?)]
          [usr-info         (place? . -> . any/c)]
          [ls               (place? symbol? . -> . (or/c list? false?))]
          [call             (place? any/c . -> . any/c)]
          [cast             (place? any/c . -> . void?)]))


;;====================================================================
;; Struct definitions
;;====================================================================

(struct InitRequest (ch pnet-mod arg-lst) #:prefab)
(struct UsrInfoRequest (ch) #:prefab)
(struct LsRequest (ch place) #:prefab)
(struct CallRequest (ch msg) #:prefab)
(struct CastRequest (msg) #:prefab)


;;====================================================================
;; API Functions
;;====================================================================

(define (start-pnet-place pnet-mod . arg-lst)

  ; start place
  (define p
    (place ch

           ; dynamically load Petri net struct
           (define-values (*PNET* arg-lst)
             (match (place-channel-get ch)
               [(InitRequest ch-send pnet-mod arg-lst)
                (place-channel-put ch-send 'ok)
                (values (dynamic-require pnet-mod '*PNET*) arg-lst)]))

           ; extract fields from loaded Petri net struct
           (define place-set (Pnet-place-set *PNET*))
           (define init-marking (Pnet-init-marking *PNET*))
           (define init (PnetPlace-init *PNET*))
           (define trigger (PnetPlace-trigger *PNET*))
  
           ; gather user info field
           (define usr-info (apply init arg-lst))
  
           ; gather initial marking
           (define marking (make-hash))
           (set-for-each place-set
                        (λ (place)
                          (let ([token-lst (init-marking place usr-info)])
                            (hash-set! marking place token-lst))))

           ; define progress-loop
           (define (progress-loop)
             
             (define delta
               (progress marking *PNET* usr-info))

             (if delta
                 (let ([delta1 (delta-apply-trigger delta
                                                    trigger
                                                    marking
                                                    usr-info)])
                   (marking-apply-delta marking delta1)
                   (progress-loop))
                 (void)))

           ; define main loop
           (define (main-loop)

             (progress-loop)

             ; process messages from outside
             (match (place-channel-get ch)
               [(UsrInfoRequest ch1)  (handle-usr-info ch1 usr-info)]
               [(LsRequest ch1 place) (handle-ls ch1 place marking)]
               [(CallRequest ch1 msg) (handle-call ch1
                                                   msg
                                                   marking
                                                   *PNET*
                                                   usr-info)]
               [(CastRequest msg)     (handle-cast msg
                                                   marking
                                                   *PNET*
                                                   usr-info)]
               [_                     (void)])


             ; recursive call
             (main-loop))

           (main-loop)))

  ; send initial message to Petri net place
  (define-values (ch-listen ch-send) (place-channel))
  (place-channel-put p (InitRequest ch-send pnet-mod arg-lst))
  (place-channel-get ch-listen) ; we're ignoring what we get back here,
                                ; we just have to make sure init has completed
  ; return place
  p)
    
(define (stop-pnet-place p)
  (place-kill p))


(define (usr-info p)

  (define-values (ch-listen ch-send)
    (place-channel))

  (place-channel-put p (UsrInfoRequest ch-send))
  (place-channel-get ch-listen))


(define (ls p place)

  (define-values (ch-listen ch-send)
    (place-channel))

  (place-channel-put p (LsRequest ch-send place))
  (place-channel-get ch-listen))


(define (call p msg)

  (define-values (ch-listen ch-send)
    (place-channel))

  (place-channel-put p (CallRequest ch-send msg))
  (place-channel-get ch-listen))

(define (cast p msg)

  (place-channel-put p (CastRequest msg)))



;;====================================================================
;; Internal Functions
;;====================================================================

(define/contract (handle-ls ch1 place marking)
  (place-channel? symbol? hash? . -> . void?)

  (place-channel-put ch1 (hash-ref marking place #f)))


(define/contract (handle-usr-info ch1 usr-info)
  (place-channel? any/c . -> . void?)

  (place-channel-put ch1 usr-info))


(define/contract (handle-call ch1 msg marking pn usr-info)
  (place-channel? any/c hash? PnetPlace? any/c . -> . void?)

  (define trigger
    (PnetPlace-trigger pn))

  (define call-handler
    (PnetPlace-handle-call pn))

  (define call-reply
    (call-handler msg marking usr-info))

  (define delta
    (CallReply-delta call-reply))

  (define reply-msg
    (CallReply-msg call-reply))

  (if delta
      (let ([delta1 (delta-apply-trigger delta trigger marking usr-info)])
        (marking-apply-delta marking delta1))
      (void))

  (place-channel-put ch1 reply-msg))
  
(define/contract (handle-cast msg marking pn usr-info)
  (any/c hash? PnetPlace? any/c . -> . void?)

  (define trigger
    (PnetPlace-trigger pn))

  (define cast-handler
    (PnetPlace-handle-cast pn))

  (define delta
    (cast-handler msg marking usr-info))

  (if delta
      (let ([delta1 (delta-apply-trigger delta trigger marking usr-info)])
        (marking-apply-delta marking delta1))
      (void)))






;;====================================================================
;; Unit Tests
;;====================================================================

(module+ test

  (require rackunit)
  
  ;; Tests to be run with raco test
  )

;;====================================================================
;; Main Sub-Module
;;====================================================================

(module+ main)


