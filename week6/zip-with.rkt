#lang racket
(require rackunit)
(require rackunit/text-ui)

; zip-with
; като zip, но не е задължително да правим двойки, а даваме функция, която да комбинира елементите - тестовете са добър източник на примери

(define (zip-with f xs ys)
  (define (zip-help xs ys)
    (cond
      ((or (null? xs) (null? ys)) '())
      (else (cons  (f (car xs) (car ys)) (zip-help (cdr xs) (cdr ys))))))
  (zip-help xs ys)
)

(define tests
  (test-suite "zip-with"
    (check-equal? (zip-with + '(1 2 3) '(4 5 6)) '(5 7 9))
    (check-equal? (zip-with * '(28 9 12) '(1 3)) '(28 27))
  )
)

(run-tests tests 'verbose)