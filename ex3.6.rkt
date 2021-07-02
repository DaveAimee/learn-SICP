#lang SICP
(#%require rackunit)
(#%require "text3.1.2.rkt")

;;rand-modifed as a variable
(define rand-modified
  (let ((x 0))
      (define (dispatch m)      
          (cond ((eq? m 'generate)
                   (set! x (rand-update x))
                          x)
                ((eq? m 'reset)
                 (lambda (new-x)
                     (set! x new-x)))
                (else (error "Illegal parameter" m))))
    dispatch))

(define rand-x
  (let ((x 0))
    (lambda (m)
      (cond ((eq? m 'generate)
                   (set! x (rand-update x))
                          x)
                ((eq? m 'reset)
                 (lambda (new-x)
                     (set! x new-x)))
                (else (error "Illegal parameter" m))))))

(rand-modified 'generate)
(rand-modified 'generate)

(rand-x 'generate)
(rand-x 'generate)