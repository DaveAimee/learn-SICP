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

(define dx 0.0001)

(define (smooth f)
  (lambda (x)
    (/ (+(f x)
         (f (- x dx))
         (f (+ x dx)))
       3)))

(define (smooth-nth n)
  (repeated smooth n))

