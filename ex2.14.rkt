#lang sicp

;;interval 
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (max (car interval)
                                    (cdr interval)))

(define (lower-bound interval) (min (car interval)
                                    (cdr interval)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c percent)
  (make-interval (- c (* c percent)) (+ c (* c percent))))

(define (percent i)
  (/ (width i) (center i)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (print-interval name i)
  (newline)
  (display name)
  (display ": [")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;;User's methods
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;Test
(define r11 (make-center-percent 10 0.5))
(define r21 (make-center-percent 30 0.5))

(define r12 (make-center-percent 10 0.4))
(define r22 (make-center-percent 30 0.4))

(define r13 (make-center-percent 10 0.3))
(define r23 (make-center-percent 30 0.3))

(define r14 (make-center-percent 10 0.2))
(define r24 (make-center-percent 30 0.2))

(define r15 (make-center-percent 10 0.1))
(define r25 (make-center-percent 30 0.1))

(define r16 (make-center-percent 10 0.05))
(define r26 (make-center-percent 30 0.05))

(define r17 (make-center-percent 10 0.001))
(define r27 (make-center-percent 30 0.001))

;;par1
(print-interval "par1-50%" (par1 r11 r21))
(print-interval "par1-40%" (par1 r12 r22))
(print-interval "par1-30%" (par1 r13 r23))
(print-interval "par1-20%" (par1 r14 r24))
(print-interval "par1-10%" (par1 r15 r25))
(print-interval "par1-5%" (par1 r16 r26))
(print-interval "par1-1%" (par1 r17 r27))

;;par2
(print-interval "par2-50%" (par2 r11 r21))
(print-interval "par2-40%" (par2 r12 r22))
(print-interval "par2-30%" (par2 r13 r23))
(print-interval "par2-20%" (par2 r14 r24))
(print-interval "par2-10%" (par2 r15 r25))
(print-interval "par2-5%" (par2 r16 r26))
(print-interval "par2-1%" (par2 r17 r27))