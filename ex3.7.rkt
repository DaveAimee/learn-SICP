#lang SICP

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch given-password m)
    (cond ((not (eq? given-password password)) (lambda (amount) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'associate) (lambda (m) (dispatch password m)))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)



(define (make-joint account password new-password)
    (define (change-to-acc given-password m)
      (cond ((not (eq? given-password new-password)) (lambda (amount) "Incorrect password"))
            (else ((account password 'associate) m))))
  change-to-acc)


(define peter-acc (make-account 100 'secret-password))

(define paul-acc
  (make-joint peter-acc 'secret-password 'rosebud))

((paul-acc 'rosebud 'withdraw) 50)
((peter-acc 'secret-password 'withdraw) 50)
((paul-acc 'rosebud 'withdraw) 50)