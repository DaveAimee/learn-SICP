(define (make-withdraw initial-amount)
	(let ((balance initial-amount))
		(lambda (amount) 
			(if (>= balance initial-amount)
				(begin (set! balance (- balance amount))
					   balance)
				"Insufficient funds"))))


(define (make-withdraw initial-amount)
	((lambda (balance) (lambda (amount) (if (>= balance initial-amount)
				                            (begin (set! balance (- balance amount))
					                               balance)
				                            "Insufficient funds"))) initial-amount))


(define W1 (make-withdraw 100))

When make-withdraw is evaluated, E0 is created with Frame A having the
initial-mount binding. Next, as a result of the evaluation of the 
anonymous function (generated by the set structure), Frame B is created
with the binding of balance (E1 is the pointer to this frame).

         _______________________
global->| make-withdraw : *     |
env.    | W1 :  *         |     |
         -------|---^-----|---^-
                |   |     |   |
                |   |     parameter: initial-mount
                |   |     body: ((lambda (balance) ((...))) initial-mount)
                |   |
                |  _|___Frame_A__________
                | | initial-mount : 100  |<- E0
                |  -^--------------------
                |   |
                |  _|__________Frame_B______
                | | balance : initial-mount | <- E1
                |  -^-----------------------
                |   |
                parameter: amount
                body: (if (>= balance amount) ... )

(W1 50)

Set! will affect Frame B, initial-mount remains unchanged in Frame A. 
         _______________________
global->| make-withdraw : *     |
env.    | W1 :  *         |     |
         -------|---^-----|---^-
                |   |     |   |
                |   |     parameter: initial-mount
                |   |     body: ((lambda (balance) ((...))) initial-mount)
                |   |
                |  _|___Frame_A__________
                | | initial-mount : 100  |<- E0
                |  -^--------------------
                |   |
                |  _|__________Frame_B___
                | | balance : 50         | <- E1
                |  -^--------------------
                |   |
                parameter: amount
                body: (if (>= balance amount) ... )