#lang sicp
(define (double x)
  (* 2 x))
(define (halve x)
  (/ x 2))
(define (even? x)
  (= (remainder x 2) 0))

(define (times a b)
  (cond ((= 1 a) b)
        ((even? a) (times (/ a 2) (* b 2)))
        (else (+ b (times (- a 1) b)))))

(times 1 2)
(times 2 3)
(times 3 8)
(times 9 9)              

