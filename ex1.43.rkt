#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x)
  (* x x))

(define (repeated f n)
  (define (repeated-iter result i)
    (if (> i 1)
        (repeated-iter (compose f result) (- i 1))
        result))
  (repeated-iter f n))

((repeated square 2) 5)


