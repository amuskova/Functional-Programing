#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да проверим дали х се съдържа в lst

(define (member? x lst)
  (define (member?-iter x lst)
    (cond
      ((and (null? (cdr lst)) (> x (car lst))) #f)
      ((= (car lst) x) #t)
      (else (member?-iter x (cdr lst)))))
  (member?-iter x lst)
)

(define tests
  (test-suite "member tests"
    (check-true (member? 2 (range 1 6)))
    (check-false (member? 22 (range 1 20)))
  )
)

(run-tests tests 'verbose)