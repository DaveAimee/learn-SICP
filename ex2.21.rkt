#lang sicp

(define (square x)
  (* x x))

(define (square-list-1st items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-1st (cdr items)))))

(square-list-1st (list 1 2 3 4))

(define (square-list-2nd items)
  (map square items))

(square-list-2nd (list 1 2 3 4))


