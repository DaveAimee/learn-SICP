#lang racket

  
 (define (install-complex-packages) 
   ;; ... 
   (put 'zero? '(complex)
        (lambda (x) (and (= (real-part x) 0)
                         (= (imag-part x) 0))))
   'done) 

 (define (install-scheme-number-packages)
   ;; ...
   (put '=zero? '(scheme-number)
        (lambda (x) (= x 0)))
   'done)

 (define (install-rational-packages)
   ;; ...
   (put '=zero? '(rational)
        (lambda (x) (and (= (numer x) 0)
                         (not (= denom x) 0))))
   'done)
 (define (=zero? x) (apply-generic '=zero? x))