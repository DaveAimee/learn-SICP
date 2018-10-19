#lang sicp

#|My solution
(define (make-rectangle vertex-a vertex-b) (cons vertex-a vertex-b))

(define (a-vertex rectangle) (car rectangle))

(define (b-vertex rectangle) (cdr rectangle))

(define (make-point x y) (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

;My solution
#|
(define (perimeter-rectangle rect) (* (+ (abs (- (x-point (a-vertex rect))
                                              (x-point (b-vertex rect))))
                                         (abs (- (y-point (a-vertex rect))
                                              (y-point (b-vertex rect))))) 2))

(define (area-rectangle rect) (* (abs (- (x-point (a-vertex rect))
                                              (x-point (b-vertex rect))))
                                         (abs (- (y-point (a-vertex rect))
                                              (y-point (b-vertex rect))))))
|#
;Clearer version
;Bug here
(define (perimeter-rectangle rect)
  (let* ((m (a-vertex rect))
        (n (b-vertex rect))
        (horizental-edge (abs (- (x-point m)
                                 (x-point n))))
        (vertical-edge (abs (-   (y-point m)
                                 (y-point n)))))
    (* (+ horizental-edge vertical-edge) 2)))

(define (area-rectangle rect)
  (let* ((m (a-vertex rect))
        (n (b-vertex rect))
        (horizental-edge (abs (- (x-point m)
                                 (x-point n))))
        (vertical-edge (abs (- (y-point m)
                                 (y-point n))))
    (* horizental-edge vertical-edge))))

(define point1 (make-point 1.5 2.5))
(define point2 (make-point 3 8))

(define rect1 (make-rectangle point1 point2))

(perimeter-rectangle rect1)
(area-rectangle rect1)
|#
;2-nd implementation start
;; Point
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
;; Rectangle-2nd implementation
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))
 ;; "Internal accessors", not to be used directly by clients.  Not sure 
 ;; how to signify this in scheme. 
 (define (bottom-left rect) (car rect)) 
 (define (bottom-right rect) 
   (make-point (x-point (cdr rect)) 
               (y-point (car rect)))) 
 (define (top-left rect) 
   (make-point (x-point (car rect)) 
               (y-point (cdr rect)))) 
 (define (top-right rect) (cdr rect)) 
  
 (define (width-rect rect) 
   (abs (- (x-point (bottom-left rect)) 
           (x-point (bottom-right rect))))) 
 (define (height-rect rect) 
   (abs (- (y-point (bottom-left rect)) 
           (y-point (top-left rect)))))

;; Public methods 
 (define (area-rect rect) 
   (* (width-rect rect) (height-rect rect))) 
 (define (perimeter-rect rect) 
   (* (+ (width-rect rect) (height-rect rect)) 2))

 ;; Usage: 
 (define r (make-rect (make-point 1 1) 
                      (make-point 3 7))) 
 (area-rect r) 
 (perimeter-rect r) 
  

