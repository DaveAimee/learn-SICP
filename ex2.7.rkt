#lang sicp
;;Inteval constructor and selector
(define (make-interval a b)
  (cons a b))

(define (upper-bound interval) (max (car interval) (cdr interval))) 
(define (lower-bound interval) (min (car interval) (cdr interval)))

;;Examples
(define interv1 (make-interval 3.5 8.5))
(define interv2 (make-interval -3.5 8.5))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(upper-bound (mul-interval interv1 interv2))

(lower-bound (mul-interval interv1 interv2))



