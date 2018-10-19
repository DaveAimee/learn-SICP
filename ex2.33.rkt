#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))
(define list1 (list 1 2 3 4 5))
(define list2 (list 6 7 8 9 10))
(define (square x)
  (* x x))

(my-map square list1)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append list1 list2)

(define (my-length sequence)
  (accumulate (lambda (first already-acc)
                (+ 1 already-acc))
              0
              sequence))
