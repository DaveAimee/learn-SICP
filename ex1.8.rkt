#lang sicp

(define (cubic-root x)
  (cubic-root-iter 0.0 1.0 x))

(define (cubic-root-iter last-guess guess x)
  (if (good-enough? last-guess guess)
      guess
      (cubic-root-iter guess (improve guess x) x)))

(define (good-enough? oldguess guess)
  (<= (abs(- guess oldguess)) (* guess 0.001)))


(define (improve guess x)
  (/ (+ (* 2 guess)
        (/ x (square guess)))
     3))

(define (square num)
  (* num num))

(cubic-root 0.003)