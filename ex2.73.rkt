#lang SICP
#| get和put函數的實現 from stackoverflow|#
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

#| 數據導向的求導程序包 |#

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

#| 將和式的項添加到表格里|#
(define (install-deriv-sum-package)
  (define (deriv-sum operands var)
    (apply make-sum (map deriv operands)))
  (put 'deriv '+ deriv-sum))

#| 將積式的項添加到表格里|#
(define (install-deriv-product-package)
  (define (deriv-product operands var)
    (make-sum
     (make-product (deriv (car operands))
                   (cdr operands))
     (make-product (deriv (cdr operands))
                   (car operands))))
  (put 'deriv '* deriv-product))
