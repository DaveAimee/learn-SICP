#lang SICP

(define f
  (let ((unknown -1/2))
    (lambda (x)
        (begin (set! unknown
                     (+ unknown x))
               unknown))))



