#lang SICP

(define (make-account balance password)
  (define incorrect-tolerance 6)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    (display "Call the cops!!!!"))
  (define (dispatch given-password m)
    (cond ((equal? incorrect-tolerance 0) (lambda (amount) (call-the-cops)))
          ((not (eq? given-password password)) (begin (set! incorrect-tolerance
                                                            (- incorrect-tolerance 1))
                                                      (lambda (amount) "Incorrect password")))
          ((eq? m 'withdraw) (begin (set! incorrect-tolerance 7)
                                    withdraw))
          ((eq? m 'deposit) (begin (set! incorrect-tolerance 7)
                                    deposit))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'random 'withdraw) 50)

((acc 'random 'withdraw) 50)

((acc 'random 'withdraw) 50)

((acc 'random 'withdraw) 50)

((acc 'random 'withdraw) 50)

((acc 'random 'withdraw) 50)

((acc 'random 'withdraw) 50)


