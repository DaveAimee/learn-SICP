#lang sicp



(define (for-each proc items)
  (let ((items-cdr (cdr items)))
    (proc (car items))
  (if (not (null? items-cdr))
      (for-each proc items-cdr)
       true)))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))




