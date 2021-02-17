#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да вземем i-тия елемент от списъка lst, като броим от 0.

(define (list-ref lst i)
  (define (loop lst i result)
    (if (<= i 0)
        result
        (loop (cdr lst) (- i 1) (car lst))))
  (loop (cdr lst) i (car lst))
)

(define tests
  (test-suite "List ref tests"
    (check-equal? (list-ref '(5 9 2) 0) 5)
    (check-equal? (list-ref '(1 8 6 2 3) 4) 3)
  )
)

(run-tests tests 'verbose)