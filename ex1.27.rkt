#lang sicp
(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (try-it n a)
  (= (expmod a n n) a))

(define (carmichael-test n)
  (define (test-iter n i)
    ( if (< i n)
         (if (try-it n i) (test-iter n (+ i 1)) false)
         true))
  (test-iter n 1))


(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2466)