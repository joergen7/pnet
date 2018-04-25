#lang typed/racket/base

;;====================================================================
;; Requirements
;;====================================================================

(require racket/list)

;;====================================================================
;; Provisions
;;====================================================================

(provide cnr
         hash-permutate
         select-random)



;;====================================================================
;; API Functions
;;====================================================================

(: cnr (All (a) ((Listof a) Nonnegative-Integer -> (Listof (Listof a)))))

(define (cnr lst n)

  (: cnr*
     (All (a) ((Listof a)
               Nonnegative-Integer
               (Listof a) -> (Listof (Listof a)))))
  
  (define (cnr* lst n acc)
    (if (zero? n) (list acc)
        (if (null? lst) '()
            (let ([h : a (first lst)]
                  [t : (Listof a) (rest lst)])
              (if (null? t)
                  (cnr* '() (sub1 n) (cons h acc))
                  (append (cnr* t (sub1 n) (cons h acc)) (cnr* t n acc)))))))

  (cnr* lst n '()))






(: hash-permutate
   (All (a b) ((HashTable a (Listof b)) -> (Listof (HashTable a b)))))

(define (hash-permutate h)

  (define key-lst : (Listof a)
    (hash-keys h))

  (for/fold ([acc : (Listof (HashTable a b)) (list (hash))])
            ([key key-lst])

    (define v-lst : (Listof b)
      (hash-ref h key))

    (define nested-lst : (Listof (Listof (HashTable a b)))
      (for/list ([a acc])
        (ann (for/list ([v v-lst])
               (hash-set a key v))
             (Listof (HashTable a b)))))

    (apply append nested-lst)))





(: select-random (All (a) ((Listof a) -> a)))

(define (select-random lst)
  (first (shuffle lst)))



;;====================================================================
;; Unit Tests
;;====================================================================

(module+ test

  (require typed/rackunit)

  (let* ([lst '(1 2 3 4)]
         [v (select-random lst)])
    (check-true (and (member v lst) #t))))

