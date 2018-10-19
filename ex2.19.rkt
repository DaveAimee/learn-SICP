#lang sicp

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (no-more? coin-values)
  (= (length coin-values) 0))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination coin-list)
  (cdr coin-list))

(define (first-denomination coin-values)
  (car coin-values))
        
(define us-coins (list 25 50 10 5 1))

(cc 100 us-coins)