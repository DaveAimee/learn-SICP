#lang sicp

(define (even? x)
  (= (remainder x 2) 0))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
#|
(define (same-parity . w)
  (let ((rem (remainder (car w) 2)))
    (define (parity-iter items result)
      (if (null? items)
          result
          (if (= (remainder (car items) 2) rem)
              (parity-iter (cdr items) (append result (cons (car items) nil)))
              (parity-iter (cdr items) result))))
    (parity-iter w nil)))
|#
(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(define (same-parity . w)
  (let ((rem (remainder (car w) 2)))
    (define (parity-iter items result)
      (if (null? items)
          (reverse result)
          (if (= (remainder (car items) 2) rem)
              (parity-iter (cdr items) (cons (car items) result))
              (parity-iter (cdr items) result))))
    (parity-iter w nil)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)