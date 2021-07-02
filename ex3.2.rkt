#lang SICP

(define (make-monitered f)
  (define times-called 0)
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) times-called)
          ((eq? m 'reset-count) (begin (set! times-called 0)
                                       times-called))
          (else (begin (set! times-called (+ times-called 1))
                       (f m)))))
  dispatch)

(define s (make-monitered sqrt))

(s 100)

(s 90)

(s 'how-many-calls?)

(s 'reset-count)

(s 90)
(s 'how-many-calls?)