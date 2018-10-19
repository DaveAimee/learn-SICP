#lang sicp
;recursive
#|(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
|#
;iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner a (term res)))))
  (iter a null-value))
         
(define (indentity x)
  x)

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

(sum identity 0 inc 3)
(product identity 1 inc 3)