#lang SICP
(#%require rackunit)
(#%require "polynomial.rkt")
(#%require "putgetlib.rkt")
(#%provide (all-defined))

(define (greatest-common-divisor v1 v2)
  (if (and (number? v1) (number? v2))
      (gcd v1 v2)
      (apply-generic 'common-divisor v1 v2)))


;(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
;(define p2 (make-polynomial 'x '((3 1) (1 -1))))
;(display "\n")
;(display (greatest-common-divisor p1 p2))
;(display "\n")
;(display (add p1 p2))
;(display "\n")
;(display (mul p1 p2))