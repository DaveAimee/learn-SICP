#lang SICP

#| leaf相关函数 |#
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))



#| append function |#
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

#| tree相关函数 |#
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

#| 解析符号 |#
(define (exist? symbol symbols)
    (cond ((null? symbols) #f)
          ((eq? symbol (car symbols)) #t)
          (else (exist? symbol (cdr symbols)))))

(define (encode-1 symbol branch bits)
  (if (leaf? branch)
      (reverse bits)
      (if (exist? symbol (symbols (left-branch branch)))
          (encode-1 symbol (left-branch branch) (cons '0 bits))
          (encode-1 symbol (right-branch branch) (cons '1 bits)))))

(define (encode-symbol symbol tree)
  (if (exist? symbol (symbols tree))
      (encode-1 symbol tree nil)
      (error "bad symbol" symbol)))
  
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
#| 生成霍夫曼编码树 |#
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

#| 样例树 |#
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
#| 样例消息 |#
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

#| 测试 |#
(encode (list 'A 'D 'A 'B 'B 'C 'A) sample-tree)
