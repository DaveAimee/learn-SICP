#lang sicp
;my solution
#|
(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(last-pair (list 23 72 149 34))
|#
;std solution
 (define (last-pair items) 
   (let ((rest (cdr items))) 
     (if (null? rest) 
         items 
         (last-pair rest))))
(last-pair (list 23 72 149 34))
