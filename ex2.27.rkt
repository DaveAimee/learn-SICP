#lang sicp
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (if (pair? (car items)) (iter (cdr items) (cons (deep-reverse (car items)) result))
                                 (iter (cdr items) (cons (car items) result)))))
  (iter items nil))



(deep-reverse x)

(list (list 4 3)(list 2 1))