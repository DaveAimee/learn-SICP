#lang SICP

;;Pseudocode
(define (install-complex-package)
  ...
  (put 'project '(complex) (lambda (x) (make-real (real-part x))))
  ...)


(define (install-real-package)
  ...
  (put 'project '(real) (lambda (x) (make-rational XXX)))
  ...)

(define (install-rational-package)
  ...
  (put 'project '(rational) (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
  ...)

(define (project number)
  (apply-generic 'project number))

(define (drop num)
  (let ((proj (project num)))
    (let ((raised (raise proj)))
      (if (equ? proj raised)
          (drop (proj num))
          num))))
;; apply-generic 
;; the only change is to apply drop to the (apply proc (map contents args))  
(drop (apply proc (map contents args))) 