#lang SICP

;;following codes are just for explainment.
;;define type tower's level
(define (level type)
	(cond ((eq? type 'integer) 0)
		  ((eq? type 'rational) 1)
		  ((eq? type 'real) 2)
		  ((eq? type 'complex) 3)
		  (else (error "Invalid type: LEVEL" type))))


;;look for max value in list
(define (find-max list func)
	(define (iter list res)
		(if (null? list)
		    res
		    (if (< (func (car list)) (func res))
		        (iter (cdr list) (car list))
		        (iter (cdr list) res))))
	(iter list (car list)))


;;transform x to super
(define (transform x super)
	(if (= (level x) (level super))
		x
		(transform (raise x) super)))

;;transform whole orign args list
(define (iter args orign-args)
    (let ((max-level-arg (find-max orign-args level)))
    	(let ((result (true-map (lambda (x) (transform x max-level-arg))
                              orign-args)))
    	result)))


(define (apply-generic op . args)
  (let ((new-args (iter args args)))
    (let ((type-tags (map type-tag new-args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents new-args))
            (error "No method for these types"
                   (list op type-tags)))))))  	

