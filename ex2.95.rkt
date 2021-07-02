#lang SICP

(#%require rackunit)
(#%require "polynomial.rkt")
(#%require "putgetlib.rkt")

(define (greatest-common-divisor v1 v2)
  (if (and (number? v1) (number? v2))
      (gcd v1 v2)
      (apply-generic 'common-divisor v1 v2)))



(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define Q1 (mul p1 p2))

(define Q2 (mul p1 p3))

(display "Q1:\n")
(display Q1)

(display "Q2:\n")
(display Q2)

(display "gcd of Q1 and Q2:\n")
(display (greatest-common-divisor Q1 Q2))

