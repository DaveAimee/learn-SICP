#lang SICP


;;Pseudocode
(define (adjoin-term term term-list)
  (cond ((=zero? term) term-list)
        ((eq? (order term) (length term-list))
         (cons (coeff term) term-list))
        (else (adjoin-term term (cons 0 term-list)))))

(define (first-term term-list) 
     (make-term (car term-list) (- (len term-list) 1) )) 