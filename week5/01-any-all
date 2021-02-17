#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме функция, която ни казва дали поне един елемент в даден списък
; изпълнява някакво условие

(define (any? p? xs)
  (define (check xs)
    (cond
      ((null? xs) #f)
      ((p? (car xs)) #t)
      (else (check (cdr xs)))))
  (check xs)
)

; Искаме функция, която ни казва дали всички елементи в списък удовлетворяват
; дадено условие
; Дефинирайте я на база на any?

(define (all? p? xs)
  (define (check-all xs)
    (cond
      ((null? xs) #t)
      ((not (p? (car xs))) #f)
      (else (check-all (cdr xs)))))
  (check-all xs)
)


(define any?-tests
  (test-suite ""
    (check-true (any? odd? '(2 4 4 2 8 9 2 0)))
    (check-true (any? (lambda (x) (> (length x) 3)) '((1 2 3) (3 4 4 2) (2 1))))
    (check-false (any? (lambda (x) (> x 2)) (map (lambda (x) (remainder x 3)) (range 1 100))))
  )
) 

(define all?-tests
  (test-suite ""
    (check-true (all? (lambda (x) (> x 100)) (range 101 103)))
  )
)

(run-tests any?-tests 'verbose)
(run-tests all?-tests 'verbose)