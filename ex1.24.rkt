#lang sicp
(#%require (lib "27.ss" "srfi"))
;fast-prime?
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;fast-prime end

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next x)
  (if (= x 2) 3 (+ x 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= (smallest-divisor n) n))


(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

;search-for-primes method 1
(define (search-for-primes-answer-one first last)
  (define (search-iter cur last)
    (if (<= cur last) (timed-prime-test cur))
    (if (<= cur last) (search-iter (+ cur 2) last)))
  (search-iter (if (even? first) (+ first 1) first)
               (if (even? last) (- last 1) last)))

;search-for-primes method 2
(define (search-for-primes-my-answer start)
  (define (search-iter start count)
    (if (> count 0) (timed-prime-test start))
    (if (> count 0) (if (prime? start) (search-iter (+ start 2) (- count 1)) (search-iter (+ start 2) count))))
  (search-iter (if (even? start) (+ start 1) start) 3))

(search-for-primes-my-answer 10000000000)  ;1e10
(search-for-primes-my-answer 100000000000) ;1e11
(search-for-primes-my-answer 1000000000000) ;1e12
(search-for-primes-my-answer 10000000000000) ;1e13
