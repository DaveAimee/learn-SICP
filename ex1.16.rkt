#lang sicp
(define (even? x)
  (= (remainder x 2) 0))

(define (modified-exp b n)
  (define (exp-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (exp-iter (* b b) (/ n 2) a))
          (else (exp-iter b (- n 1) (* a b)))))
  (exp-iter b n 1))

(modified-exp 2 3)
(modified-exp 3 6)
(modified-exp 5 4)

