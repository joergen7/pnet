#lang racket

(provide pnet-interface
         start)

(define pnet-interface
  (interface () place-lst trsn-lst preset enabled? fire init-marking))

(define (loop marking pnet)
  ; TODO
  (loop marking pnet))

(define (start pnet)

  (define marking0
    (for/fold
     ([marking (hash)])
     ([place (send pnet place-lst)])
     (hash-set marking place (send pnet init-marking place))))
      
  (define pnet-thread
    (thread (lambda () (loop marking0 pnet))))
  
  (thread-send pnet-thread 'continue))


(define cvm%
  (class* object% (pnet-interface)

    (define/public (place-lst)
       '(coin-slot cash-box signal storage compartment))
    
    (define/public (trsn-lst)
       '(a b))

    (define/public (preset trsn)
      (match trsn
        ['a '(coin-slot)]
        ['b '(signal storage)]))

    (define/public (enabled? trsn marking)
      #t)
    
    (define/public (fire trsn mode)
      (match trsn
        ['a (list 'produce (hash 'cash-box '(coin) 'signal '(sig)))]
        ['b (list 'produce (hash 'compartment '(cookie-box)))]))
    
    (define/public (init-marking trsn)
      (match trsn
        ['storage '(cookie-box cookie-box cookie-box)]
        [_        '()]))

    (super-new)

  ))






(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )




