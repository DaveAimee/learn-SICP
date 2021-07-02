#lang SICP


;;add into polynomial package
(define (=zero? x)
  (define (poly? x)
    (pair? x))
  (cond ((number? x) (= x 0))
        ((poly? x) false)
        (else (error "Unknown type" x))))

(put '=zero? 'polynomial =zero?)


        
