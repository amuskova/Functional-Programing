#lang racket
(require rackunit)
(require rackunit/text-ui)

; chunk 
; разбива списъка xs на подсписъци с дължина n
(define (length xs)
  (define (length-iter xs result)
    (cond
      ((null? xs) result)
      (else
       (length-iter (cdr xs) (+ result 1)))))
  (length-iter xs 0)
)   

(define (take n xs)
  (cond
    ((null? xs) '())
    ((> n (length xs)) xs)
    ((= n 1) (cons (car xs) '()))
    (else
     (cons (car xs) (take (- n 1) (cdr xs)))))

 )
(define (drop n xs)
  (cond
    ((null? xs) '())
    ((> n (length xs)) '())
    ((> n 0) (drop (- n 1) (cdr xs)))
    (else
     xs)))

(define (chunk n xs)
  (cond
    ((null? xs) '())
    (else
     (append (list (take n xs)) (chunk n (drop n xs)))))
)

(define tests
  (test-suite "chunk"
    (check-equal? (chunk 2 '(1 2 3 4 5 6 7 8 9))  '((1 2) (3 4) (5 6) (7 8) (9)))
    (check-equal? (chunk 3 '(1 2 3 4 5 6 7 8 9))  '((1 2 3) (4 5 6) (7 8 9)))
  )
)

(run-tests tests 'verbose)