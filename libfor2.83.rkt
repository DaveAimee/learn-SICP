#lang SICP
(#%provide (all-defined))
;;put and get lib
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

(define global-array '())


 ;;put-coercion and get-coercion lib
(define coercion-list '())

(define (clear-coercion-list)
  (set! coercion-list '()))

(define (put-coercion type1 type2 item)
  (if (get-coercion type1 type2) coercion-list 
      (set! coercion-list
            (cons (list type1 type2 item)
                  coercion-list))))

(define (get-coercion type1 type2) 
  (define (get-type1 listItem)
    (car listItem))
  (define (get-type2 listItem)
    (cadr listItem))
  (define (get-item listItem)
    (caddr listItem))
  (define (get-coercion-iter list type1 type2)
    (if (null? list) #f
        (let ((top (car list)))
          (if (and (equal? type1 (get-type1 top))
                   (equal? type2 (get-type2 top))) (get-item top)
                   (get-coercion-iter (cdr list) type1 type2)))))
  (get-coercion-iter coercion-list type1 type2))


;;functions related to apply-generic
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
	(if (pair? datum)
	(car datum)
	(error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
	(if (pair? datum)
	(cdr datum)
	(error "Bad tagged datum -- CONTENTS" datum)))

(define (transform x a)
  (if (equal? (type-tag x) (type-tag a))
      x
      (let ((type-x->type-a (get-coercion (type-tag x) (type-tag a))))
        (if type-x->type-a
            (type-x->type-a x)
            false))))

(define (apply-generic op . args)
  (define (iter args orign-args)
    (let ((first-ele (car args)))
      (let ((result (true-map (lambda (x) (transform x first-ele))
                              orign-args)))
        (if (null? args)
            (error "No method for correct transmission")
            (if (eq? result false)
                (iter (cdr args)
                      orign-args)
                result)))))
  (let ((new-args (iter args args)))
    (let ((type-tags (map type-tag new-args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents new-args))
            (error "No method for these types"
                   (list op type-tags)))))))

(define (true-map func list)
  (define (iter list res)
    (if (null? list)
          (reverse res)
          (let ((e (car list)))
            (if (eq? (func e) false)
                false
                (iter (cdr list)
                      (cons (func e)
                            res))))))
  (iter list '()))

;;square 
(define (square x) (* x x))

;;polar package
(define (install-polar-package)
	;; internal procedures
	(define (magnitude z) (car z))
	(define (angle z) (cdr z))
	(define (make-from-mag-ang r a) (cons r a))
	(define (real-part z)
	(* (magnitude z) (cos (angle z))))
	(define (imag-part z)
	(* (magnitude z) (sin (angle z))))
	(define (make-from-real-imag x y)
	(cons (sqrt (+ (square x) (square y)))
	(atan y x)))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'polar x))
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(put 'make-from-real-imag 'polar
	(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar
	(lambda (r a) (tag (make-from-mag-ang r a))))
'done)


;;rectangular package
(define (install-rectangular-package)
	;; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (magnitude z)
	(sqrt (+ (square (real-part z))
	(square (imag-part z)))))
	(define (angle z)
	(atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
	(cons (* r (cos a)) (* r (sin a))))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular
	(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular
	(lambda (r a) (tag (make-from-mag-ang r a))))
'done)

;;real-part,image-part,magnitude,angle
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;;complex package

;complex包
(define (install-complex-package)
	;; imported procedures from rectangular and polar packages
	(define (make-from-real-imag x y)
		((get 'make-from-real-imag 'rectangular) x y))
	(define (make-from-mag-ang r a)
  		((get 'make-from-mag-ang 'polar) r a))
	;; internal procedures
	(define (add-complex z1 z2)
		(make-from-real-imag (+ (real-part z1) (real-part z2))
		(+ (imag-part z1) (imag-part z2))))
	(define (sub-complex z1 z2)
		(make-from-real-imag (- (real-part z1) (real-part z2))
		(- (imag-part z1) (imag-part z2))))
	(define (mul-complex z1 z2)
		(make-from-mag-ang (* (magnitude z1) (magnitude z2))
		(+ (angle z1) (angle z2))))
	(define (div-complex z1 z2)
		(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		(- (angle z1) (angle z2))))
	;; interface to rest of the system
	(define (tag z) (attach-tag 'complex z))
	(put 'add '(complex complex)
		(lambda (z1 z2) (tag (add-complex z1 z2))))
	(put 'sub '(complex complex)
		(lambda (z1 z2) (tag (sub-complex z1 z2))))
	(put 'mul '(complex complex)
		(lambda (z1 z2) (tag (mul-complex z1 z2))))
	(put 'div '(complex complex)
		(lambda (z1 z2) (tag (div-complex z1 z2))))
	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)
	(put 'make-from-real-imag 'complex
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'complex
		(lambda (r a) (tag (make-from-mag-ang r a))))
'done)




;;create complex
(define (make-complex-from-real-imag x y)
	((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
	((get 'make-from-mag-ang 'complex) r a))
;(define (make-from-mag-ang r a)
;	((get 'make-from-mag-ang 'polar) r a))

;(define (make-from-real-imag x y)
;	((get 'make-from-real-imag 'rectangular) x y))  

;;scheme-number package
(define (install-scheme-number-package)
	(define (tag x)
		(attach-tag 'scheme-number x))
	(put 'add '(scheme-number scheme-number)
	(lambda (x y) (tag (+ x y))))
	(put 'sub '(scheme-number scheme-number)
	(lambda (x y) (tag (- x y))))
	(put 'mul '(scheme-number scheme-number)
	(lambda (x y) (tag (* x y))))
	(put 'div '(scheme-number scheme-number)
	(lambda (x y) (tag (/ x y))))
	(put 'make 'scheme-number
	(lambda (x) (tag x)))
'done)

;;create scheme-number
(define (make-scheme-number n)
((get 'make 'scheme-number) n))


;;rational package
;rational包
(define (install-rational-package)
;; internal procedures
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat n d)
		(let ((g (gcd n d)))
		(cons (/ n g) (/ d g))))
	(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
	(* (numer y) (denom x)))
	(* (denom x) (denom y))))
	(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
	(* (numer y) (denom x)))
	(* (denom x) (denom y))))
	(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
	(* (denom x) (denom y))))
	(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
	(* (denom x) (numer y))))
	;; interface to rest of the system
	(define (tag x) (attach-tag 'rational x))
	(put 'add '(rational rational)
	(lambda (x y) (tag (add-rat x y))))
	(put 'sub '(rational rational)
	(lambda (x y) (tag (sub-rat x y))))
	(put 'mul '(rational rational)
	(lambda (x y) (tag (mul-rat x y))))
	(put 'div '(rational rational)
	(lambda (x y) (tag (div-rat x y))))
	(put 'make 'rational
	(lambda (n d) (tag (make-rat n d))))
'done)
;create rational
(define (make-rational n d)
	((get 'make 'rational) n d))


;;calculation
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;coercion package
(define (install-coercion-package)
	(define (scheme-number->complex x)
		(make-complex-from-real-imag (contents x) 0))
	(define (scheme-number->rational x)
		(make-rational (contents x) 1))
	(put-coercion 'scheme-number 'complex scheme-number->complex)
	(put-coercion 'scheme-number 'rational scheme-number->rational)
'done)

;;Test
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-complex-package)
(install-rational-package)
(install-coercion-package)
#|
(display "Testing complex package\n")
(define number1 (make-complex-from-real-imag 3 4))
(real-part number1)
(imag-part number1)
(magnitude number1)
(angle number1)
(display "Finished\n")

(display "Testing scheme-number\n")
(define scheme1 (make-scheme-number 2))
(define scheme2 (make-scheme-number 3))
(add scheme1 scheme2)
(sub scheme1 scheme2)
(mul scheme1 scheme2)
(div scheme1 scheme2)
(display "Finished\n")

(display "Testing rational package\n")
(define rational1 (make-rational 1 3))
(define rational2 (make-rational 4 9))
(add rational1 rational2)
(sub rational1 rational2)
(mul rational1 rational2)
(div rational1 rational2)
(display "Finished\n")
|#







