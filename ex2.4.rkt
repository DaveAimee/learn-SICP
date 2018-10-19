#lang racket


(define (cons-alter x y)
  (lambda (m) (m x y)))

(define (car-alter z)
  (z (lambda (p q) p)))

(define (cdr-alter z)
  (z (lambda (p q) q)))

(define pair (cons-alter 1 2))

(car-alter pair)
(cdr-alter pair)






           