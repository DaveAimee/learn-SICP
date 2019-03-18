#lang SICP

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else "Unknown op -- MAKE-FROM-MAG-ANG" op)))
  dispatch)

(define (apply-generic op arg) (arg op))

(define z (make-from-mag-ang 6 0.52))

(apply-generic 'magnitude z)
(apply-generic 'angle z)
(apply-generic 'real-part z)
(apply-generic 'imag-part z)