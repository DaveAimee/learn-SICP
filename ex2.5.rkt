#lang sicp

;; Logarithmic iteration
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (cons-alter x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(define (car-alter z)
  (continuous-divide-by-base 2 z))
         
(define (cdr-alter z)
  (continuous-divide-by-base 3 z))

(define (square x)
  (* x x))

(define (continuous-divide-by-base base product)
  (define (iter product count)
    (if (= (remainder product base) 0)
        (iter (/ product base) (+ count 1))
        count))
  (iter product 0))


(car-alter (cons-alter 5 9))
(cdr-alter (cons-alter 5 9))