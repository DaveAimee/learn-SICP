#lang sicp

;interval methods
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (max (car interval)
                                    (cdr interval)))

(define (lower-bound interval) (min (car interval)
                                    (cdr interval)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c percent)
  (make-interval (- c (* c percent)) (+ c (* c percent))))

(define (percent i)
  (/ (width i) (center i)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))



(define (print-interval name i)
  (newline)
  (display name)
  (display ": [")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define i (make-center-percent 100 0.05))

(percent i)
(print-interval "i" i)