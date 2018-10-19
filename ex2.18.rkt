#lang sicp
;iterative 
#|
(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(reverse (list 1 4 9 16 25))
|#

;recursive
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse list)
  (if (null? (cdr list))
      list
      (append (reverse (cdr list)) (cons (car list) nil))))


                    
        

(reverse (list 1 4 9 16 25))



