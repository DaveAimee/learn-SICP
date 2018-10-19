#lang sicp
(#%require (lib "27.ss" "srfi"))
;filtered-accumulate
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a res)
    (if (> a b)
        res
        (if (filter a)
            (iter (next a) (combiner (term a) res))
            (iter (next a) res))))
  (iter a null-value))
  
;prime?
(define (square x)
  (* x x))

(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt x squaremod)
      (if (and (= squaremod 1)
               (not (= x 1))
               (not (= x (- m 1))))
          0
          squaremod))
    (check-nontrivial-sqrt x (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (squaremod-with-check
                      (miller-rabin-expmod base (/ exp 2) m)))
        (else (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                         m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0))
           (= x 1)))
    (check-it (miller-rabin-expmod a  (- n 1) n)))
  (try-it (+ (random (- n 1)) 1)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (if (= n 1) false
              (fast-prime? n 100)))

;Test example
(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-of-prime-squares 1 5)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (identity x)
  x)

(define (sum-of-relative-prime n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime?))

(sum-of-relative-prime 10)
