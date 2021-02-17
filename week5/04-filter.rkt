#lang racket
(require rackunit)
(require rackunit/text-ui)

; filter
; приема едноместен предикат и списък
; връща списък само с елементите на оригиналния, които изпълняват условието

(define (filter p? ys)
  (define (filter-i ys)
    (cond
      ((null? ys) '())
      ((p? (car ys)) (cons (car ys) (filter-i (cdr ys))))
      (else (filter-i(cdr ys)) )))
  (filter-i ys)
)

(define tests
  (test-suite "filter"
    (check-equal? (filter even? '(1 2 3))  '(2))
    (check-equal? (filter (lambda (x) (> x 200)) '(1 2 3))  '())
    (check-equal? (filter (lambda (x) (or (= x 1) (= x 3))) '(1 2 3))  '(1 3))
  )
)

(run-tests tests 'verbose)
