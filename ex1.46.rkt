#lang sicp

(define tolerance 0.00001)

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (iter n)
       (if (good-enough? n)
            n
            (iter (improve n))))
      (iter x)))

(define (close-enough? v1 v2) 
   (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (x) (close-enough? x (f x)))
                       f)
    first-guess))

(define (exact-enough? oldguess guess)
  (<= (abs(- guess oldguess)) (* guess 0.0001)))

(define (average x1 x2)
  (/ (+ x1 x2) 2))

(define (sqrt-improve x)
  (lambda (y) (average (/ x y) y)))

(define (sqrt x)
  ((iterative-improve (lambda (y) (exact-enough? y ((sqrt-improve x) y)))
                      (sqrt-improve x)) 1.0))

(sqrt 2)
(fixed-point cos 1.0)


