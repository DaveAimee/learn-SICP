#lang sicp

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 3))



(define (fringe tree)
  (define (build-fringe x result)
    (cond ((null? x) result)
          ((not (pair? x)) (cons x result))
          (else (build-fringe (car x)
                              (build-fringe (cdr x)
                                            result)))))
  (build-fringe tree nil))

(fringe x)






(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))
  