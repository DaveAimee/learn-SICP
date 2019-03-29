#lang SICP

(define (install-scheme-number-package)

    ;; internal procedures
    (define (tag x)
        (attach-tag 'scheme-number x))

    ;; interface to rest of the system
    (put 'make 'scheme-number
        (lambda (x)
            (tag x)))

    ;; 新增
    (put 'equ? '(scheme-number scheme-number)
        (lambda (x y)
            (= x y)))

'done)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

(define (install-rational-package)
    
    ;; internal procedures
    (define (numer x)
        (car x))

    (define (denom x)
        (cdr x))

    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))

    ;; interface to rest of the system
    (define (tag x)
        (attach-tag 'rational x))

    (put 'make 'rational
        (lambda (n d)
            (tag (make-rat n d))))

    ;; 新增
    (put 'equ? '(rational rational)
        (lambda (x y)
            (and (= (numer x) (numer y))
                 (= (denom x) (denom y)))))

'done)

(define (make-rational n d)
    ((get 'make 'rational) n d))

(define (install-complex-package)

    ;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    ;; interface to rest of the system
    (define (tag z)
        (attach-tag 'complex z))

    (put 'make-from-real-imag 'complex
        (lambda (x y)
            (tag (make-from-real-imag x y))))

    (put 'make-from-mag-ang 'complex
        (lambda (r a)
            (tag (make-from-mag-ang r a))))
    
    ;; 补充完整缺少的选择函数(练习 2.77)
    (put 'real-part '(complex) real-part)

    (put 'imag-part '(complex) imag-part)

    (put 'magnitude '(complex) magnitude)

    (put 'angle '(complex) angle)

    ;; 新增
    (put 'equ? '(complex complex)
        (lambda (x y)
            (and (= (real-part x) (real-part y))
                 (= (imag-part x) (imag-part y)))))

    ;; equ? 的另一种实现，对比 magnitude 和 angle

    ; (put 'equ? '(complex complex)
    ;    (lambda (x y)
    ;        (and (= (magnitude x) (magnitude x))
    ;             (= (angle x) (angle y)))))

'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))

(define (equ? x y)
  (apply-generic 'equ? x y))