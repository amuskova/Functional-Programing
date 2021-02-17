#lang racket
(require rackunit)
(require rackunit/text-ui)

; Ще сортираме списък по метода на пряката селекция.
; За тази цел започваме с дефиниции на спомагателни функции.

; remove-first - връща ни xs, но без първото срещане на x
(define (remove-first x xs)
  (cond ((null? xs) '())
        ((= x (car xs)) (cdr xs))
        (else (cons (car xs) (remove-first x (cdr xs))))))


; find-min - връща ни най-малкото число от непразен списък
(define (find-min xs)
  (cond ((null? (cdr xs)) (car xs))
        (else (min (car xs) (find-min (cdr xs))))))


(define (selection-sort xs)
  (if (null? xs)
      '()
      (cons (find-min xs) (selection-sort (remove-first (find-min xs) xs))))) 

(define tests
  (test-suite "Selection sort"
    ; Искаме да тестваме тази функция
    (check-equal? (selection-sort '(5 9 2 1 8 2 1)) '(1 1 2 2 5 8 9))
    (check-equal? (selection-sort '()) '())
    (check-equal? (selection-sort '(-56 -96 -32 8 -15 0 7)) '(-96 -56 -32 -15 0 7 8))
  )
)

(run-tests tests 'verbose)
