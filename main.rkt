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
          [usr-info         (place? . -> . any/c)]
          [ls               (place? symbol? . -> . (or/c list? false?))]
          [call             (place? any/c . -> . any/c)]))


;;====================================================================
;; Struct definitions
;;====================================================================

(struct InitRequest (ch pnet-mod arg-lst) #:prefab)
(struct UsrInfoRequest (ch) #:prefab)
(struct LsRequest (ch place) #:prefab)
(struct CallRequest (ch msg) #:prefab)


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
                 (begin
                   (marking-apply-delta marking delta)
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
                                                   usr-info)])


             ; recursive call
             (main-loop))

           (main-loop)))

  ; send initial message to Petri net place
  (define-values (ch-listen ch-send) (place-channel))
  (place-channel-put p (InitRequest ch-send pnet-mod arg-lst))
  (place-channel-get ch-listen)

  ; return place
  p)
    
    

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

  (define call-handler
    (PnetPlace-handle-call pn))

  (define call-reply
    (call-handler msg marking usr-info))

  (define delta
    (CallReply-delta call-reply))

  (define reply-msg
    (CallReply-msg call-reply))

  (if delta
      (marking-apply-delta marking delta)
      (void))

  (place-channel-put ch1 reply-msg))
  







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


