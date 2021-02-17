#lang racket
(require rackunit)
(require rackunit/text-ui)

; Обръщаме число в двоична бройна система

(define (to-binary number)
  (define (to-binary-iter num pos res)
    (if (= num 0)
        res
        (to-binary-iter (quotient num 2) (+ pos 1) (+ res (* (remainder num 2) (expt 10 pos))))
        )
    
  )
  (to-binary-iter number 0 0)
)
;(to-binary 10)
(define tests
  (test-suite "to-binary tests"
    (check-equal? (to-binary 10) 1010)
    (check-equal? (to-binary 0) 0)
    (check-equal? (to-binary 8) 1000)
  )
)

(run-tests tests 'verbose)