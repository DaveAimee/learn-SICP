#lang sicp

(define (square x)
  (* x x))

(define (tan-cf x k)
  (define (tan-rec i)
    (let ((numerator (if (= i 1) x (square x)))
          (a (- (* 2 i) 1)))
      (if (> i k)
          0
          (/ numerator (- a (tan-rec (+ i 1)))))))
  (tan-rec 1))

(tan-cf 0.785398 5)
        
