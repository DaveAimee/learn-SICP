#lang sicp
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
  (if (prime? n)
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
;1e10 average-time: 31225   next:20834    ratio:1.49
;1e11 average-time: 125001  next:67733    ratio:1.84
;1e12 average-time: 370116  next:229099   ratio:1.61
;1e13 average-time: 1166632 next:692617   ratio:1.68
;增长基本符合√10的预期 ratio低于2