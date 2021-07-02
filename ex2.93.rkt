#lang SICP

(#%require rackunit)
(#%require "putgetlib.rkt")
(#%require "polynomial.rkt")

;rational包
(define (install-rational-package)
;; internal procedures
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat n d)
          (cons n  d))
	(define (add-rat x y)
	(make-rat (add (mul (numer x) (denom y))
	(mul (numer y) (denom x)))
	(mul (denom x) (denom y))))
	(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
	(* (numer y) (denom x)))
	(* (denom x) (denom y))))
	(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
	(* (denom x) (denom y))))
	(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
	(* (denom x) (numer y))))
	;; interface to rest of the system
	(define (tag x) (attach-tag 'rational x))
	(put 'add '(rational rational)
	(lambda (x y) (tag (add-rat x y))))
	(put 'sub '(rational rational)
	(lambda (x y) (tag (sub-rat x y))))
	(put 'mul '(rational rational)
	(lambda (x y) (tag (mul-rat x y))))
	(put 'div '(rational rational)
	(lambda (x y) (tag (div-rat x y))))
	(put 'make 'rational
	(lambda (n d) (tag (make-rat n d))))
'done)
;create rational
(define (make-rational n d)
	((get 'make 'rational) n d))

;;gcd on polynomials
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (/ (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2))))
                  (let ((rest-of-result (div-terms (add-terms L1
                                                              (negate-terms (mul-term-by-all-terms (make-term new-o new-c)
                                                                                             L2)))
                                                   L2)))
                    (list (adjoin-term (make-term new-o new-c)
                                       (car rest-of-result))
                          (cadr rest-of-result))))))))
(define (negate-terms termlist)
  (if (empty-termlist? termlist)
      (the-empty-termlist)
      (let ((t (first-term termlist)))
        (adjoin-term (make-term (order t) (- (coeff t)))
                     (negate-terms (rest-terms termlist))))))

(define (remainder-terms a b)
  (cadr (div-terms a b)))

(install-rational-package)
(display "\n")

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

(define terms1 '((2 1) (0 1)))
(define terms2 '((3 1) (0 1)))

(display (div-terms terms2 terms1))
(display "\n")
(display (remainder-terms terms2 terms1))
(display "\n")

(display (add rf rf))