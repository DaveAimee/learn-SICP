#lang sicp
(define (even? x)
  (= (remainder x 2) 0))

(define (peasant-method a b sum)
  (cond ((= a 0) sum)
        ((even? a) (peasant-method (floor (/ a 2)) (* b 2) sum))
        (else (peasant-method (floor (/ a 2)) (* b 2) (+ sum b)))))

(define (times a b)
  (peasant-method a b 0))

(times 3 2)


 
