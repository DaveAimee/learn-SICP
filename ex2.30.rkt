#lang sicp

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-1st tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-1st (car tree))
                    (square-tree-1st (cdr tree))))))
x
(square-tree-1st x)

(define (square-tree-2nd tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2nd sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree-2nd x)