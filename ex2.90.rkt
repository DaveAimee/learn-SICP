#lang SICP
(#%require rackunit)
(#%require "table.rkt")
;;procedures related to tag
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum))) 

;;functions relates to term
(define (install-term-package)
  (define (order term)
    (car term))
  (define (coeff term)
    (cadr term))
  (define (make-term order coeff)
    (list order coeff))
  (define (tag x)
    (attach-tag 'term x))
  ;;
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'make-term 'term
       (lambda (x y) (tag (make-term x y))))
'term-done)

(define (order term)
  (apply-generic 'order term))

(define (coeff term)
  (apply-generic 'coeff term))

(define (make-term order coeff)
  ((get 'make-term 'term) order coeff))



;;sparse polynomial package
(define (install-sparse-package)
  ;;internal procedures
  (define (tag x)
    (attach-tag 'sparse x))
  (define (order term)
    (car term))
  (define (coeff term)
    (cadr term))
  (define (adjoin-term term term-list)
    (if (= (coeff term) 0)
        term-list
        (cons (attach-tag 'term term) term-list)))
  (define (the-empty-termlist)
    '())
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  ;;interfaces
  (put 'adjoin-term '(term sparse)
       (lambda (x y) (tag (adjoin-term x y))))
  (put 'first-term '(sparse)
       (lambda (x)  (first-term x)))
  (put 'rest-terms '(sparse)
       (lambda (x) (tag (rest-terms x))))
'sparse-done)

;;dense polynomial package
(define (install-dense-package)
  ;;internal procedures
  (define (tag x)
    (attach-tag 'dense x))
  (define (order term)
    (car term))
  (define (coeff term)
    (cadr term))
  (define (adjoin-term term term-list)
    (cond ((= (coeff term) 0) term-list)
        ((eq? (order term) (length term-list))
         (cons (coeff term) term-list))
        (else (adjoin-term term (cons 0 term-list)))))
  (define (the-empty-termlist)
    '())
  (define (first-term term-list)
    (make-term (car term-list) (- (length term-list) 1) ))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  ;;
  (put 'adjoin-term '(term dense)
       (lambda (x y) (tag (adjoin-term x y))))
  (put 'first-term '(dense)
       (lambda (x) (first-term x))) 
  (put 'rest-terms '(dense)
       (lambda (x) (tag (rest-terms x))))
'dense-done)

(define (first-term L)
  (apply-generic 'first-term L))

(define (rest-terms L)
  (apply-generic 'rest-terms L))

(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term term term-list))

(install-term-package)

(define t1 (make-term 3 3))

(define t2 (make-term 2 2))

(define t3 (make-term 1 9))

(install-sparse-package)

;;L1:3x^3+2x^2
(define L1 (adjoin-term t1 (attach-tag 'sparse (list t2))))
(display L1)
(display "\n")

(display (first-term L1))
(display "\n")

(display (rest-terms L1))
(display "\n")
 
;;L2: 3x^3+2x^2+9x
(install-dense-package)
(define L2 (attach-tag 'dense (list 3 2 9 0)))
(display L2)
(display "\n")

(display (first-term L2))
(display "\n")

(display (rest-terms L2))
(display "\n")


