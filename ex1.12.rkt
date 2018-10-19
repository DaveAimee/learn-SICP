#lang sicp

(define (pascal-recursive m n)
  (cond ((= m n) 1)
        ((= n 0) 1)
        (else (+ (pascal-recursive (- m 1) (- n 1)) (pascal-recursive (- m 1) n)))))

(define (triangle-iter row col counter maxcount)
  (cond (())

(define (triangle rownum)
  (
  