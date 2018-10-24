#lang sicp

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


#| 生成霍夫曼编码树 |#
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

#| 构造出叶的初始排序集合 |#
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                                    (cddr leaf-set)))))
#| 生成Huffman树 |#
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

#| encode函数 |#
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

#| 测试 |#
(define tree (generate-huffman-tree '((A 1) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define msg-1 '(GET A JOB))

(encode msg-1 tree)



