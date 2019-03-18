#lang SICP
#| get和put函数的实现 from stackoverflow|#
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

#| 求导常用操作 |#
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          (else (list '* m1 m2))))

(define (first-element s) (car s))

(define (second-element s) (cadr s))
#| 数据导向的求导程序包 |#

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

#| 將和式的項添加到表格里|#
(define (install-deriv-sum-package)
  ;;internal procedures
  (define (deriv-sum s var)
    (make-sum (deriv (first-element s) var)
              (deriv (second-element s) var)))
  (put 'deriv '+ deriv-sum)
'done)

(install-deriv-sum-package)

(deriv '(+ x 3) 'x)
(deriv '(+ x (+ x 3)) 'x)

#| 将积式添加到表格里 |#
(define (install-deriv-product-package)
  ;;internal procedures
  (define (deriv-product s var)
    (make-sum (make-product (deriv (first-element s) var)
                            (second-element s))
              (make-product (deriv (second-element s) var)
                            (first-element s))))
  (put 'deriv '* deriv-product)
'done)

(install-deriv-product-package)

(deriv '(+ (* 3 x) 2) 'x)

(deriv '(* x y) 'x)

#| 将乘方添加到表格里 |#
(define (install-deriv-exponent-package)
  (define (base exp)
    (car exp))
  (define (exponent exp)
    (cadr exp))
  (define (make-exponent base expo)
    (cond ((=number? expo 0) 1)
          ((=number? expo 1) base)
          (else (list '** base expo))))
  (define (deriv-exponent s var)
    (let ((u (base s))
          (n (exponent s)))
      (make-product n
                    (make-product (make-exponent u (- n 1))
                                  (deriv u var)))))
  (put 'deriv '** deriv-exponent)
  'done)

(install-deriv-exponent-package)
(deriv '(** x 1) 'x)
