#lang SICP

(define (make-accumulator sum)
  (lambda (value)
    (begin (set! sum (+ sum value))
           sum)))

(define A (make-accumulator 5))

(A 10)

(A 10)
