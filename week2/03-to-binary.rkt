#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (to-binary number)
  

)
(define tests
  (test-suite "to binary tests"
              (check-equal? (to-binary 10) 1010)
              (check-equal? (to-binary 0) 0)
              (check-equal? (to-binary 8) 1000)
  )
)