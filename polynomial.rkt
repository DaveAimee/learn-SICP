#lang SICP
(#%provide (all-defined))
(#%require rackunit)
(#%require "putgetlib.rkt")
;;procedures related to tag
;;(define (attach-tag type-tag contents)
;;  (cons type-tag contents))

;;(define (type-tag datum)
;;    (if (pair? datum)
;;        (car datum)
;;        (error "Bad tagged datum -- TYPE-TAG" datum)))

;;(define (contents datum)
;;(if (pair? datum)    
;;(cdr datum)
;;    (error "Bad tagged datum -- CONTENTS" datum))) 

;;is x a variable
(define (variable? x)
  (symbol? x))

;;are v1 and v2 the same variables
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;add terms
(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
          (cond ((> (order t1) (order t2))
                 (adjoin-term
                  t1 (add-terms (rest-terms L1) L2)))
                ((< (order t1) (order t2))
                 (adjoin-term
                  t2 (add-terms L1 (rest-terms L2))))
                (else
                 (adjoin-term
                  (make-term (order t1)
                             (+ (coeff t1) (coeff t2)))
                  (add-terms (rest-terms L1)
                             (rest-terms L2)))))))))


;;mul terms
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))


(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (* (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))



;;polynomial package
(define (install-polynomial-package)
  ;;representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (great-common-divisor p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1)
                                           (term-list p2)))
        (error "Polys not in same var -- GCD-POLY")))
  ;;interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'common-divisor '(polynomial polynomial)
       (lambda (x y) (tag (great-common-divisor x y))))
'done)


;;create term representation
(define (adjoin-term term term-list)
  (if (= (coeff term) 0)
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())

(define (first-term term-list) (car term-list))

(define (rest-terms term-list) (cdr term-list))

(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))

(define (order term) (car term))

(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

(display "normal polynomial package loaded")



;;a
(define (gcd-terms-a a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))

;;b
(define (gcd-terms a b)
  (if (empty-termlist? b)
      (let* ((coeff-list (map cadr a))
             (gcd-coeff (apply gcd coeff-list)))
        (first-term (div-terms a (list (make-term 0 gcd-coeff)))))
      (gcd-terms b (pseudoremainder-terms a b))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (/ (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2))))
                  (let ((rest-of-result (div-terms (add-terms L1
                                                              (negate-terms (mul-term-by-all-terms (make-term new-o new-c)
                                                                                             L2)))
                                                   L2)))
                    (list (adjoin-term (make-term new-o new-c)
                                       (car rest-of-result))
                          (cadr rest-of-result))))))))

(define (negate-terms termlist)
  (if (empty-termlist? termlist)
      (the-empty-termlist)
      (let ((t (first-term termlist)))
        (adjoin-term (make-term (order t) (- (coeff t)))
                     (negate-terms (rest-terms termlist))))))

(define (pseudoremainder-terms a b)
  (let ((O1 (order (first-term a)))
        (O2 (order (first-term b)))
        (c (coeff (first-term b))))
    (let ((new-a (mul-term-by-all-terms (make-term 0 (expt c (+ 1 (- O1 O2))))
                                        a)))
      (cadr (div-terms new-a b)))))
     
                                                        
(define (remainder-terms a b)
  (cadr (div-terms a b)))


