#lang racket

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
;getters
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
; validation
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))
(define empty-tree? null?)


(define example (make-tree 1
                           (make-tree 3 (make-leaf 8) empty-tree)
                           (make-tree 7 empty-tree (make-tree 9
                                                              (make-leaf 10)
                                                              (make-leaf 11)))))

; (invert tree)
;     4               4
;    / \   invert    / \
;   2   5 ========> 5   2
;  / \                 / \
; 1   3               3   1

(define (invert tree)
  (if (empty-tree? tree)
      empty-tree
      (make-tree (root-tree tree) (right-tree tree) (left-tree tree)))
)
(define inverttree (make-tree 4
                              (make-tree 2 (make-leaf 1) (make-leaf 3))
                              (make-leaf 5)))
; прави пълно дърво с дадена височина и всичко стойности във върховете са дадената стойност
(define (full-tree level value)
  (cond
    ;((empty-tree? tree) empty-tree)
    ((= level 0) (make-leaf value))
    (else
     (make-tree value (full-tree (- level 1) value) (full-tree (- level 1) value))))
)
(define-syntax my-delay
  (syntax-rules () ((my-delay expr) (lambda () expr))))

(define (my-force p) (p))

(define-syntax stream-cons
  (syntax-rules () ((stream-cons x xs) (cons x (my-delay xs)))))

(define (head s) (car s))
(define (tail s) (my-force (cdr s)))
(define empty-stream '())
(define (stream-empty? s)
  (null? s))

; поток от всички пълни дървета с дадена стойност
(define (full-trees value)
  (define (make-full value level)
    (stream-cons (full-tree level value) (make-full value (+ level 1))))
  (make-full value 0)
)

; Да се напише функция extremum, която по даден списък от списъци от числа намира число, което е минимално или максимално във всеки от списъците, ако има такова, или 0 иначе
; (extremum '((1 2 3 2) (3 5) (3 3) (1 1 3 3))) → 3

(define (extremum xs)
  (define (findmin xs a)
    (cond
      ((null? xs) a)
      ((< (car xs) a) (findmin (cdr xs) (car xs)))
      (else
       (findmin (cdr xs) a))))
  (define (findmax xs a)
    (cond
      ((null? xs ) a)
      ((> (car xs) a) (findmax (cdr xs) (car xs)))
      (else
       (findmax (cdr xs) a))))
  
   
)