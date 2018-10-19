#lang sicp

#|fixed-point start|#
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average v1 v2)
  (/ (+ v1 v2) 2))
#|fixed-point end|#

#|average-damp start|#
(define (average-damp f)
  (lambda (x) (average x (f x))))
#|average-damp end|#

#|repeated start|#
(define (compose f g)
  (lambda (x)
    (f (g x))))



(define (repeated f n)
  (define (repeated-iter result i)
    (if (> i 1)
        (repeated-iter (compose f result) (- i 1))
        result))
  (repeated-iter f n))

#|repeated end|#

#|fast-expt start|#

 (define (fast-expt b p) 
   (define (even? x) 
     (= (remainder x 2) 0)) 
    
   (define (sqr x) 
     (* x x)) 
    
   (define (iter res a n) 
     (if (= n 0) 
         res 
         (if (even? n) 
             (iter res (sqr a) (/ n 2)) 
             (iter (* res a) a (- n 1))))) 
    
   (iter 1 b p))

#|fast-expt end|#

#|nth-root start|#
(define (log2 x) (/ (log x) (log 2))) 
(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (lambda (y) (/ x (fast-expt y (- n 1)))))
               1.0))
 
#|nth-root end|#
(nth-root 5.0 10)

