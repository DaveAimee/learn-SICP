#lang SICP

(#%require rackunit)
(#%require "putgetlib.rkt")

(define (true-map func list)
  (define (iter list res)
    (if (null? list)
          (reverse res)
          (let ((e (car list)))
            (if (eq? (func e) false)
                false
                (iter (cdr list)
                      (cons (func e)
                            res))))))
  (iter list '()))

(define (add x y) (apply-generic 'add x y))
;(define (sub x y) (apply-generic 'sub x y))
;(define (mul x y) (apply-generic 'mul x y))
;(define (div x y) (apply-generic 'div x y))


(define (transform x a)
  (if (equal? (type-tag x) (type-tag a))
      x
      (let ((type-x->type-a (get-coercion (type-tag x) (type-tag a))))
        (if type-x->type-a
            (type-x->type-a x)
            false))))

(define (apply-generic op . args)
  (define (iter args orign-args)
    (let ((first-ele (car args)))
      (let ((result (true-map (lambda (x) (transform x first-ele))
                              orign-args)))
        (if (null? args)
            (error "No method for correct transmission")
            (if (eq? result false)
                (iter (cdr args)
                      orign-args)
                result)))))
  (let ((new-args (iter args args)))
    (let ((type-tags (map type-tag new-args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (error "No method for these types"
                   (list op type-tags)))))))



          




