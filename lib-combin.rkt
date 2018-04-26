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

(: cnr (All (a) ((Listof a) Natural -> (Listof (Listof a)))))

(define (cnr lst n)

  (: cnr*
     (All (a) ((Listof a)
               Natural
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

  (check-equal? (select-random '(a)) 'a
    "select-from singleton list returns only element")

  (check-exn exn:fail:contract? (λ () (select-random '()))
    "select-from empty list throws exception")

  (let* ([lst : (Listof Natural) '(1 2 3 4)]
         [v   : Natural          (select-random lst)])

    (check-true (and (member v lst) #t)
      "select-random returns a member of its argument list"))

  (let* ([lst : (Listof Symbol) '(a b c d e f)])

    (check-equal? (length (cnr lst 1)) 6
      "(cnr lst 1) produces n combinations")

    (check-equal? (length (cnr lst 6)) 1
      "(cnr lst n) produces 1 element")

    (check-equal? (cnr lst 0) '(())
      "(cnr lst 0) is degenerate but valid")

    (check-equal? (cnr lst 7) '()
      "(cnr lst x) where x > n returns empty list"))

  (check-equal?
    (hash-permutate (ann (hash) (HashTable Symbol (Listof Symbol))))
    (list (ann (hash) (HashTable Symbol Symbol)))
    "permutating empty hash returns list with single empty hash")

  (check-equal?
    (hash-permutate (ann (hash 'b '()) (HashTable Symbol (Listof Symbol))))
    '()
    "permutating map with single empty list value returns empty list")

  (check-equal?
    (hash-permutate (hash 'a '(x y) 'b '() 'c '(m n)))
    '()
    "permutating map containing even one empty list value returns empty list")

  (let* ([ingredient-hash : (HashTable Symbol (Listof Symbol))
           (hash 'sauce '(ketchup mayo)
                 'bread '(sesame plain)
                 'meat  '(beef chicken mutton))]
         [expected-lst : (Listof (HashTable Symbol Symbol))
           (list (hash 'bread 'sesame 'meat 'beef    'sauce 'ketchup)
                 (hash 'bread 'plain  'meat 'beef    'sauce 'ketchup)
                 (hash 'bread 'sesame 'meat 'chicken 'sauce 'ketchup)
                 (hash 'bread 'plain  'meat 'chicken 'sauce 'ketchup)
                 (hash 'bread 'sesame 'meat 'mutton  'sauce 'ketchup)
                 (hash 'bread 'plain  'meat 'mutton  'sauce 'ketchup)
                 (hash 'bread 'sesame 'meat 'beef    'sauce 'mayo)
                 (hash 'bread 'plain  'meat 'beef    'sauce 'mayo)
                 (hash 'bread 'sesame 'meat 'chicken 'sauce 'mayo)
                 (hash 'bread 'plain  'meat 'chicken 'sauce 'mayo)
                 (hash 'bread 'sesame 'meat 'mutton  'sauce 'mayo)
                 (hash 'bread 'plain  'meat 'mutton  'sauce 'mayo))]
         [result : (Listof (HashTable Symbol Symbol))
           (hash-permutate ingredient-hash)])
    (check-equal? result expected-lst "burger restaurant example"))
  )


