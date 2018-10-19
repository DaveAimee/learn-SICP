#lang sicp

(define tolerance 0.00001)

(define (square x)
  (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
    
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt 2)

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a (square x))
       (* b x)
       c)))
(define (third-order a b c)
  (newtons-method (cubic a b c) 1.0))

(third-order 0 0 -3)


