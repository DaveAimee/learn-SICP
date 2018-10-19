#lang sicp
(define (inc n) (+ n 1))
(define (identity x) x)

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (product-iteration term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-term n)
  (if (even? n) (/ (+ n 2) (+ n 1)) (/ (+ n 1) (+ n 2))))

(define pi
  (* 4 (product-iteration product-term 1 inc 6)))

pi


      
      


      

