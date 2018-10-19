#lang sicp
;recursive version
#|
(define (cont-frac n d k)
  (define (frac-rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (frac-rec (+ i 1))))))
    (frac-rec 1))
|#
;iterative version
(define (cont-frac n d k)
  (define (frac-iter i res)
    (let ((next-result (/ (n (- (inc k) i))
                          (+ res (d (- (inc k) i))))))
      (if (> i k)
          res
          (frac-iter (inc i) next-result))))
  (frac-iter 1 0.0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) (cond ((= (remainder i 3) 0) 1)
                             ((= (remainder i 3) 1) 1)
                             ((= (remainder i 3) 2) (* 2 (/ (+ i 1) 3)))))
           15)



