#lang racket
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

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

(define example-bst (make-tree 8
                               (make-tree 3 (make-leaf 1) (make-tree 6 (make-leaf 4) (make-leaf 7)))
                               (make-tree 10 empty-tree (make-tree 14 (make-leaf 13) empty-tree))))

(define (member-tree? x tree)
  (cond ((empty-tree? tree) #f)
        ((equal? x (root-tree tree)) #t)
        (else (or (member-tree? x (left-tree tree)) (member-tree? x (right-tree tree)))))
)


(define (sum-tree tree)
  (cond ((empty-tree? tree) 0)
        (else (+ (root-tree tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree)))))
)

(define (tree-level n tree)
  (cond ((empty-tree? tree) '())
        ((= n 0) (list (root-tree tree)))
        (else (append (tree-level (- n 1) (left-tree tree)) (tree-level (- n 1) (right-tree tree)))))
)


(define (tree-map f tree)
  (if (empty-tree? tree) empty-tree
      (make-tree (f (root-tree tree))
              (tree-map f (left-tree tree))
              (tree-map f (right-tree tree)))
  )
)

(define (tree->list tree)
  (cond ((null? tree) '())
        (else (append (tree->list (left-tree tree))
                      (list (root-tree tree))
                      (tree->list (right-tree tree)))))
)

(define (bst-member? x tree)
  (cond ((empty-tree? tree) #f)
        ((equal? x (root-tree tree)) #t)
        ((< x (root-tree tree)) (bst-member? x (left-tree tree)))
        (else (bst-member? x (right-tree tree))))
)

(define (bst-insert x tree)
  (cond ((empty-tree? tree) (make-leaf x))
	((> x (root-tree tree)) (bst-insert x (right-tree tree)))
	(else (bst-insert x (left-tree tree))))
)

(define example-list '(1 5 4 6 2 8 7))

(define (sort xs)
  (tree->list (foldr bst-insert empty-tree xs))
)