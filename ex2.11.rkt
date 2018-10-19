#lang sicp

;; This looks a *lot* more complicated to me, and with the extra 
;; function calls I'm not sure that the complexity is worth it.
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (max (car interval)
                                    (cdr interval)))

(define (lower-bound interval) (min (car interval)
                                    (cdr interval)))

(define (print-interval name i)
  (newline)
  (display name)
  (display ": [")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define i (make-interval 2 7))
(define j (make-interval 8 3))

(print-interval "i" i)
(print-interval "j" j)

