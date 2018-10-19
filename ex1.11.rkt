#lang sicp
;recursive-f
(define (recursive-f n)
  (cond ((< n 3) n)
        (else (+ (recursive-f (- n 1))
                 (* 2 (recursive-f (- n 2)))
                 (* 3 (recursive-f (- n 3)))))))
;(recursive-f 0)
;(recursive-f 1)
;(recursive-f 2)
;(recursive-f 3)

;iterative-f
(define (iterative-f n)
  (f-iter 0 1 2 n))

(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter b c (+ (* 3 a) (* 2 b) c) (- count 1))))
;(iterative-f 0)
;(iterative-f 1)
;(iterative-f 2)
;(iterative-f 3)
;(iterative-f 4)
                 
