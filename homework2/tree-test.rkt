#lang racket/base
(require rackunit rackunit/gui)
(require racket/string)
(require racket/stream)
(require "tree.rkt")

(define (correct-expr? str)
  (test-true
    (string-append "String '" str "' is valid")
    (tree? str)
  )
)

(define (wrong-expr? str)
  (test-false
    (string-append "String '" str "' is not valid")
    (tree? str)
  )
)

(define string-empty-tree "  *")
(define string-leaf-123 "{123 * *}")
(define string-tree-left-leaf " { 123 { 10 **} * } ")
(define string-tree-right-leaf " {123 *   {2 * * }  } ")
(define string-tree-two-leaves " { 123 { 10 *  *} { 2 *  * }  } ")
(define string-long-tree " { 123 { 10 { 1 { 4 * *} {3 * {3 * *}} { 5 {2 **}* } {100 {4 * *}* } ")

(define wrong-string-empty-tree "{}")
(define wrong-string-leaf-123 "{123}")
(define wrong-string-tree-left-leaf " {123 {10 } *")
(define wrong-string-tree-right-leaf " {123* {10 }")
(define wrong-string-tree-two-leaves " { 123 { 10 *  } { 2 *  * }  } ")
(define wrong-string-tree-letter "{ 123 * a* }")
(define wrong-string-tree-negative "{ -123 * * }")

(test/gui 
 (test-suite
  "Expect true in tree?"
  (correct-expr? string-leaf-123)
  (correct-expr? string-tree-left-leaf)
  (correct-expr? string-tree-right-leaf)
  (correct-expr? string-tree-two-leaves)
  (correct-expr? string-long-tree)
  (correct-expr? string-empty-tree)
 )

 (test-suite
  "Expect false in tree?"
  (wrong-expr? wrong-string-empty-tree)
  (wrong-expr? wrong-string-leaf-123)
  (wrong-expr? wrong-string-tree-left-leaf)
  (wrong-expr? wrong-string-tree-right-leaf)
  (wrong-expr? wrong-string-tree-two-leaves)
  (wrong-expr? wrong-string-tree-letter)
  (wrong-expr? wrong-string-tree-negative)
 )
)

(define tree-empty       '())
(define leaf-123         '(123 () ()))
(define tree-two-leaves  '(34 (5 () ()) (10 () ())))
(define tree-left-leaf   '(34 (5 () ()) ()))
(define tree-right-leaf  '(34 () (5 () ())))
(define tree-long        '(111 (1 (4 () ()) ()) (5 (11 () ()) ())))

(define wrong-tree-empty            '(()))
(define wrong-leaf-123               123)
(define wrong-leaf-123-brackets     '(123))
(define wrong-tree-without-right    '(123 ()))
(define wrong-tree-leaves           '(123 (4) (3)))
(define wrong-tree-more-leaf-digits '(123 (4 3) ()))

(test/gui 
 (test-suite
  "Validation for tree is expected to be true"
  (test-true "Empty tree should be valid tree" (is-tree-valid? tree-empty))
  (test-true "Leaf 123 should be valid tree" (is-tree-valid? leaf-123))
  (test-true "Tree with two leaves should be valid tree" (is-tree-valid? tree-two-leaves))
  (test-true "Tree with left leaf and without right leaf should be valid tree" (is-tree-valid? tree-left-leaf))
  (test-true "Tree with right leaf and without left leaf should be valid tree" (is-tree-valid? tree-right-leaf))
  (test-true "Long tree (111 (1 (4 () ()) ()) (5 (11 () ()) ())) should be valid tree" (is-tree-valid? tree-long))
 )

 (test-suite
  "Validation for tree is expected to be false"
  (test-false "Empty tree like (()) should not be valid" (is-tree-valid? wrong-tree-empty))
  (test-false "Leaf like 123 should not be valid" (is-tree-valid? wrong-leaf-123))
  (test-false "Leaf like (123) should not be valid" (is-tree-valid? wrong-leaf-123-brackets))
  (test-false "Tree like (123 ()) should not be valid" (is-tree-valid? wrong-tree-without-right))
  (test-false "Tree like (123 (4) (3)) should not be valid" (is-tree-valid? wrong-tree-leaves))
  (test-false "Tree like (123 (4 3) ()) should not be valid" (is-tree-valid? wrong-tree-more-leaf-digits))
 )
)

(test/gui 
 (test-suite
  "Expect correct tree->string"
   (test-equal? "Empty tree has to be converted to *" (tree->string tree-empty) "*")
   (test-equal? "Leaf 123 has to be converted to {123 * *}" (tree->string leaf-123) "{123 * *}")
   (test-equal? "Tree with root 34 and leaves 5 and 10 has to be converted to {34 {5 * *} {10 * *}}" (tree->string tree-two-leaves) "{34 {5 * *} {10 * *}}")
   (test-equal? "Tree with root 34 and left leaf 5 has to be converted to {34 {5 * *} *}" (tree->string tree-left-leaf) "{34 {5 * *} *}")
   (test-equal? "Tree with root 34 and right leaf 5 has to be converted to {34 * {5 * *}}" (tree->string tree-right-leaf) "{34 * {5 * *}}")
   (test-equal? "Tree like (111 (1 (4 () ()) ()) (5 (11 () ()) ()))) has to be converted to {111 {1 {4 * *} *} {5 {11 * *} *}}}" (tree->string tree-long) "{111 {1 {4 * *} *} {5 {11 * *} *}}")
 )
)

(define balanced-long-tree '(5 (3 (2 () ()) (4 () ())) (7 (6 () ()) ())))

(define unbalanced-tree-only-left-subtree '(5 (2 (3 () ()) ()) ()))
(define unbalanced-tree-only-right-subtree '(5 () (2 (3 () ()) ())))
(define unbalanced-tree-first '(34 (123 (10 () ()) (11 (5 (8 () ()) ()) ())) ()))
(define unbalanced-tree-second '(18 (23 (20 () ()) (100 (2 () ()) (5 () ()))) (34 () (123 (11 () ()) ()))))

(test/gui 
 (test-suite
  "Expect balanced tree"
   (test-true "Empty tree should be balanced" (balanced? tree-empty))
   (test-true "Every leaf should be balanced" (balanced? leaf-123))
   (test-true "Tree with one left leaf and empty right tree should be balanced" (balanced? tree-left-leaf))
   (test-true "Tree with empty left tree and one right leaf should be balanced" (balanced? tree-right-leaf))
   (test-true "Tree with two leaves should be balanced" (balanced? tree-two-leaves))
   (test-true "Tree (5 (3 (2 () ()) (4 () ())) (7 (6 () ()) ())) should be balanced" (balanced? balanced-long-tree))
 )

 (test-suite
  "Expect unbalanced tree"
   (test-false "Tree with left subtree with more than 2 nodes and empty right subtree has to be unbalanced" (balanced? unbalanced-tree-only-left-subtree))
   (test-false "Tree with empty left subtree and right subtree with more than 2 nodes has to be unbalanced" (balanced? unbalanced-tree-only-right-subtree))
   (test-false "Tree like (34 (123 (10 () ()) (11 (5 (8 () ()) ()) ())) ()) has to be unbalanced" (balanced? unbalanced-tree-first))
   (test-false "Tree like (18 (23 (20 () ()) (100 (2 () ()) (5 () ()))) (34 () (123 (11 () ()) ()))) has to be unbalanced" (balanced? unbalanced-tree-second))
 )
)

(define ordered-tree-left-leaf '(123 (2 () ()) ()))
(define ordered-tree-right-leaf '(123 () (125 () ())))
(define ordered-tree-two-leaves '(123 (2 () ()) (125 () ())))
(define binary-search-tree '(5 (3 (2 (1 () ()) ()) (4 () ())) (7 (6 () ()) (9 () ()))))

(define unordered-tree-left-leaf '(123 (125 () ()) ()))
(define unordered-tree-right-leaf '(123 () (2 () ())))
(define unordered-tree-two-leaves '(123 (125 () ()) (2 () ())))
(define unordered-tree '(5 (1 (2 (9 () ()) (14 () ())) (23 (19 () ()) ())) (72 (54 () (67 () ())) (76 () ()))))

(test/gui 
 (test-suite
  "Expect ordered tree"
   (test-true "Empty tree has to be ordered" (ordered? tree-empty))
   (test-true "Every leaf has to be ordered" (ordered? leaf-123))
   (test-true "Tree with left smaller leaf and empty right leaf has to be ordered" (ordered? ordered-tree-left-leaf))
   (test-true "Tree with empty left leaf and bigger right leaf has to be ordered" (ordered? ordered-tree-right-leaf))
   (test-true "Tree with smaller left leaf and bigger right leaf has to be ordered" (ordered? ordered-tree-two-leaves))
   (test-true "Every binary search tree has to be ordered" (ordered? binary-search-tree))
 )

 (test-suite
  "Expect unordered tree"
   (test-false "Tree with left bigger leaf and empty right leaf has to be ordered" (ordered? unordered-tree-left-leaf))
   (test-false "Tree with empty left leaf and smaller right leaf has to be ordered" (ordered? unordered-tree-right-leaf))
   (test-false "Tree with bigger left leaf and smaller right leaf has to be ordered" (ordered? unordered-tree-two-leaves))
   (test-false "Tree like (5 (1 (2 (9 () ()) (14 () ())) (23 (19 () ()) ())) (72 (54 () (67 () ())) (76 () ()))))" (ordered? unordered-tree))
 )
)

(define stream-123       (stream 123))
(define stream-1.2.3.4.5 (stream 1 2 3 4 5))

(test/gui 
 (test-suite
  "Streams expect to be equal"
   (test-true "Empty streams has to be equal" (are-streams-equal? empty-stream empty-stream))
   (test-true "Stream (1 2 3 4 5) and stream (1 2 3 4 5) has to be equal" (are-streams-equal? stream-1.2.3.4.5 (stream 1 2 3 4 5)))
 )

 (test-suite
  "Streams expect to be not equal"
   (test-false "Stream (1 2 3 4 5) and stream (1 2 3 5 4) has to not be equal" (are-streams-equal? stream-1.2.3.4.5 (stream 1 2 3 5 4)))
 )
)

(define tree '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))

(define preordered-stream-tree  (stream 10 15 2 7 5 22 2 6 1 3 111))
(define inordered-stream-tree   (stream 2 15 7 10 2 22 6 5 1 111 3))
(define postordered-stream-tree (stream 2 7 15 2 6 22 111 3 1 5 10))

(define actual-preordered-stream-tree  (tree->stream tree 'preorder))
(define actual-inordered-stream-tree   (tree->stream tree 'inorder))
(define actual-postordered-stream-tree (tree->stream tree 'postorder))

(define preordered-empty-stream  (tree->stream tree-empty 'preorder))
(define inordered-empty-stream   (tree->stream tree-empty 'inorder))
(define postordered-empty-stream (tree->stream tree-empty 'postorder))

(define preordered-stream-leaf-123  (tree->stream leaf-123 'preorder))
(define inordered-stream-leaf-123   (tree->stream leaf-123 'inorder))
(define postordered-stream-leaf-123 (tree->stream leaf-123 'postorder))

(test/gui 
 (test-suite
  "Expect correct preordered stream of tree"
   (test-true "Preordered stream of empty tree should be empty stream" (are-streams-equal? empty-stream preordered-empty-stream))
   (test-true "Preordered stream of leaf should be stream with leaf value" (are-streams-equal? (stream 123) preordered-stream-leaf-123))
   (test-true "Preordered stream of tree (10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) should be correct"
               (are-streams-equal? preordered-stream-tree
                                   actual-preordered-stream-tree))
 )
)

(test/gui 
 (test-suite
  "Expect correct inordered stream of tree"
   (test-true "Inordered stream of empty tree should be empty stream" (are-streams-equal? empty-stream inordered-empty-stream))
   (test-true "Inordered stream of leaf should be stream with leaf value" (are-streams-equal? (stream 123) inordered-stream-leaf-123))
   (test-true "Inordered stream of tree (10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) should be correct"
               (are-streams-equal? inordered-stream-tree
                                   actual-inordered-stream-tree))
 )
)

(test/gui 
 (test-suite
  "Expect correct postordered stream of tree"
   (test-true "Postordered stream of empty tree should be empty stream" (are-streams-equal? empty-stream postordered-empty-stream))
   (test-true "Postordered stream of leaf should be stream with leaf value" (are-streams-equal? (stream 123) postordered-stream-leaf-123))
   (test-true "Postordered stream of tree (10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) should be correct"
               (are-streams-equal? postordered-stream-tree
                                   actual-postordered-stream-tree))
 )
)