#lang SICP
(#%require rackunit)
(#%require "libfor2.83.rkt")


;;these codes are unrunnable
(define (install-integer-package)
  ( ...
    ...
   (put 'raise '(integer)
        (lambda (x) (make-rational ...)))
   ...))

(define (install-rational-package)
  ( ...
    ...
    (put 'raise '(rational)
         (lamdba (x) (make-real-number ...)))))

(define (install-real-number-package)
  ( ...
    ...
    (put 'raise '(real-number)
         (lamda (x) (make-complex ...)))))

(define (install-complex-package)
  ( ...
    ...
    (put 'raise '(complex)
         (lambda (x) x))))

(define (raise number)
  (apply-generic 'raise number))

