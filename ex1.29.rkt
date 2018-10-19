#lang sicp
;My solution
#|(define (even? x)
  (= (remainder x 2) 0))

(define (cube x)
  (* x x ))

(define (simpson f a b)
  (define (numeric-method n)
    (define (simpson-term k h)
      (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
            ((even? k) (* 2 (f (+ a (* k h)))))
            (else (* 4 (f (+ a (* k h)))))))
    (define (sum-part term k)
      (if (> k n)
          0
          (+ (term k (/ (- b a) n))
             (sum-part term (+ k 1))))) 
    (* (/ (/ (- b a) n) 3)
       (sum-part simpson-term 0)))
  (numeric-method 100))

(simpson cube 0 1)
|#
;online solution
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (round-up-to-even n)
  (+ n (remainder n 2)))

(define (simpson f a b n)
  (define fixed-n (round-up-to-even n))     ;rename a value
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (cond ((or (= k 0)
            (= k fixed-n)) y)
          ((even? k) (* 2 y))
          (else (* 4 y))))
  (* (/ h 3)
     (sum simpson-term 0 inc fixed-n)))

(define (cube x)
  (* x x x))

(simpson cube 0 1 100)
                      
  
  
  
