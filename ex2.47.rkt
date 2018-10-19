#lang sicp

(define (make-frame-1st origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-1st frame)
  (car frame))

(define (edge1-frame-1st frame)
  (car (cdr frame)))

(define (edge2-frame-1st frame)
  (car (cdr (cdr frame))))

(define (make-frame-1nd origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2nd frame)
  (car frame))

(define (edge1-frame-2nd frame)
  (car (cdr frame)))

(define (edge2-frame-2nd frame)
  (cdr (cdr frame)))
