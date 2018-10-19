#lang racket

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set)
                               (adjoin-set x (cdr set))))))

(adjoin-set 1 (list 1 3 5 7))

(adjoin-set 6 (list 1 3 5 7))

(adjoin-set 8 (list 1 3 5 7))