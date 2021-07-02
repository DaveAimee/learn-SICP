#lang SICP
(#%require rackunit)
(#%require "polynomial.rkt")

(define (reduce-terms n d)
  (let* ((gcd-factor (gcd-terms n d))
         (nn (first-term (div-terms n gcd-factor)))
         (dd (first-term (div-terms d gcd-factor))))
    (list nn dd)))
        

(define (reduce-poly p1 p2) 
   (if (same-variable? (variable p1) (variable p2)) 
     (let ((result (reduce-terms (term-list p1) (term-list p2)))) 
       (list (make-poly (variable p1) (car result)) 
             (make-poly (variable p1) (cadr result)))) 
     (error "not the same variable--REDUCE-POLY" (list p1 p2)))) 