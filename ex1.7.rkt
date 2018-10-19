#lang sicp

(define (modified-sqrt x)
  (modified-sqrt-iter 0.0 1.0 x))

(define (modified-sqrt-iter last-guess guess x)
  (if (good-enough? last-guess guess)
      guess
      (modified-sqrt-iter guess (improve guess x) x)))

(define (good-enough? oldguess guess)
  (<= (abs(- guess oldguess)) (* guess 0.001)))



(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  ( / (+ x y) 2))

(modified-sqrt 0)