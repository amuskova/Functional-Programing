#lang racket
(require rackunit)
(require rackunit/text-ui)

; remove-duplicates 
(define (remove-first x xs)
  (cond ((null? xs) '())
        ((= x (car xs)) (cdr xs))
        (else (cons (car xs) (remove-first x (cdr xs))))))


(define (find-min xs)
  (cond ((null? (cdr xs)) (car xs))
        (else (min (car xs) (find-min (cdr xs))))))


(define (selection-sort xs)
  (if (null? xs)
      '() 
      (cons (find-min xs) (selection-sort (remove-first (find-min xs) xs)))))

(define (remove-duplicates xs)
  (define (loop xs)
    (cond
      ((null? (cdr xs)) xs)
      ((= (car xs) (cadr xs)) (loop (cdr xs)))
      (else (cons (car xs) (loop (cdr xs))))))
  (loop (selection-sort xs))
)

(define tests
  (test-suite "remove-duplicates"
    (check-equal? (remove-duplicates '(1 1 2 2 1 3 3 2 3))  '(1 2 3))
    (check-equal? (remove-duplicates '(1 2 3))  '(1 2 3))
  )
)

(run-tests tests 'verbose)