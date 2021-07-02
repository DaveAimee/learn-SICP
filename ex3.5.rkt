#lang SICP

(#%require rackunit)
(#%require "text3.1.2.rkt")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (lambda () (P (random-in-range x1 x2)
                                  (random-in-range y1 y2)))))
(define (square x)
  (* x x))

(define (unit-circle-predict x y)
  (if (<= (+ (square x)
            (square y))
         1)
      true
      false))

(* (estimate-integral unit-circle-predict -1 1 -1 1 100000)
    4.0)




             