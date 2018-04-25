#lang typed/racket/base

;;====================================================================
;; Requirements
;;====================================================================

(require racket/set
         "lib-combin.rkt")

;;====================================================================
;; Provisions
;;====================================================================

(provide ;; Type Definitions
         Marking
         Mode
 
         ;; Struct Definitions
         (struct-out Delta)
         (struct-out Pnet)
         (struct-out PnetPlace)
         (struct-out CallReply)
         
         ;; API Functions
         
         ; Operations on Markings
         marking-apply-delta
         marking-add
         marking-remove
         marking-firing-modes

         attempt-progress
         progress)


;;====================================================================
;; Type Definitions
;;====================================================================

(define-type Marking
  (Mutable-HashTable Symbol (Listof Any)))

(define-type Mode
  (HashTable Symbol (Listof Any)))

(define-type Firing
  (U Mode #f))
  

;;====================================================================
;; Struct Definitions
;;====================================================================

(struct Delta ([consume : Mode]
               [produce : Mode]))

(struct Pnet ([place-set    : (Setof Symbol)]
              [trsn-set     : (Setof Symbol)]
              [preset-hash  : (HashTable Symbol (Listof Symbol))]
              [init-marking : (Symbol Any -> (Listof Any))]
              [enabled?     : (Symbol Mode Any -> Boolean)]
              [fire         : (Symbol Mode Any -> Firing)]))

(struct PnetPlace Pnet ([init        : (Any * -> Any)]
                        [handle-call : (Any Marking Any -> CallReply)]))

(struct CallReply ([msg   : Any]
                   [delta : (U Delta #f)]))


;;====================================================================
;; API Functions
;;====================================================================

;; Operations on Markings

(: marking-apply-delta (Marking Delta -> Void))
(define (marking-apply-delta marking delta)

  (define consume : Mode
    (Delta-consume delta))

  (define produce : Mode
    (Delta-produce delta))

  (marking-remove marking consume)
  (marking-add marking produce))

(: marking-add (Marking Mode -> Void))
(define (marking-add marking delta)

  (: add-proc (Symbol -> Void))
  (define (add-proc place)
    (hash-set! marking
               place
               (append (hash-ref delta place) (hash-ref marking place))))
  
  (for-each add-proc (hash-keys delta)))


(: marking-remove (Marking Mode -> Void))
(define (marking-remove marking delta)

  (: diff ((Listof Any) (Listof Any) -> (Listof Any)))
  (define (diff v-lst lst)
    (for/fold ([acc lst])
              ([v v-lst])
      (remove v acc)))

  (: remove-proc (Symbol -> Void))
  (define (remove-proc place)
    (hash-set! marking
               place
               (diff (hash-ref delta place) (hash-ref marking place))))
  
  (for-each remove-proc (hash-keys delta)))


(: marking-firing-modes (Marking (Listof Symbol) -> (Listof Mode)))
(define (marking-firing-modes marking preset)

  ; gather count hash
  (define count-hash : (HashTable Symbol Positive-Integer)
    (for/fold ([acc : (HashTable Symbol Positive-Integer) (hash)])
              ([place preset])
      
      (define n : Nonnegative-Integer
        (hash-ref acc place (λ () 0)))
      
      (hash-set acc place (add1 n))))

  ; enumerate drawing combinations for each preset place individually
  (define cmb-hash : (HashTable Symbol (Listof (Listof Any)))
    (for/fold ([acc : (HashTable Symbol (Listof (Listof Any))) (hash)])
              ([place preset])

      (define n : Positive-Integer
        (hash-ref count-hash place))

      (define token-lst : (Listof Any)
        (hash-ref marking place))

      (define cmb : (Listof (Listof Any))
        (cnr token-lst n))

      (hash-set acc place cmb)))
  
  ; enumerate permutations of hash containing drawing combinations
  (hash-permutate cmb-hash))


(: attempt-progress ((HashTable Symbol (Listof Mode)) Pnet Any -> (U Delta #f)))
(define (attempt-progress mode-hash pn usr-info)

  (define fire : (Symbol Mode Any -> Firing)
    (Pnet-fire pn))
  
  (if (hash-empty? mode-hash)
      #f
      (let* ([trsn-lst : (Listof Symbol) (hash-keys mode-hash)]
             [trsn     : Symbol          (select-random trsn-lst)]
             [mode-lst : (Listof Mode)   (hash-ref mode-hash trsn)]
             [mode     : Mode            (select-random mode-lst)]
             [firing   : Firing          (fire trsn mode usr-info)])
        (if firing
            (Delta mode firing)
            (if (equal? (length mode-lst) 1)
                (attempt-progress (hash-remove mode-hash trsn) pn usr-info)
                (attempt-progress (hash-set mode-hash
                                            trsn
                                            (remove mode mode-lst))
                                  pn
                                  usr-info))))))
            
        
(: progress (Marking Pnet Any -> (U Delta #f)))
(define (progress marking pn usr-info)

  (define trsn-lst : (Listof Symbol)
    (set->list (Pnet-trsn-set pn)))

  (define preset-hash : (HashTable Symbol (Listof Symbol))
    (Pnet-preset-hash pn))

  (define enabled? : (Symbol Mode Any -> Boolean)
    (Pnet-enabled? pn))

  (define mode-hash : (HashTable Symbol (Listof Mode))
    (for/fold ([acc : (HashTable Symbol (Listof Mode)) (hash)])
              ([trsn trsn-lst])

      (define preset : (Listof Symbol)
        (hash-ref preset-hash trsn))

      (define mode-lst : (Listof Mode)
        (marking-firing-modes marking preset))

      (: enabled?* (Mode -> Boolean))
      (define (enabled?* mode)
        (enabled? trsn mode usr-info))

      (define enabled-mode-lst : (Listof Mode)
        (filter enabled?* mode-lst))

      (if (null? enabled-mode-lst)
          acc
          (hash-set acc trsn enabled-mode-lst))))

  (attempt-progress mode-hash pn usr-info))
    
            

;;====================================================================
;; Unit Tests
;;====================================================================

(module+ test

  (require typed/rackunit)

  (let ([marking : Marking (make-hash '((x . (1 2))))])
    (marking-add marking (hash 'x '(1)))
    (check-equal? (hash-ref marking 'x) '(1 1 2) "Element should be added"))

  (let ([marking : Marking (make-hash '((x . (1 2))))])
    (marking-remove marking (hash 'x '(1)))
    (check-equal? (hash-ref marking 'x) '(2) "Only element should be removed"))
  
  (let ([marking : Marking (make-hash '((x . (1 2 1))))])
    (marking-remove marking (hash 'x '(1)))
    (check-equal? (hash-ref marking 'x) '(2 1) "One element should be removed"))

  )
