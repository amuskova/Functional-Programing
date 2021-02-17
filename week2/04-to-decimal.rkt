#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (to-decimal number)
  (define (count-digits number)
    (if(< number 0)
       (count-digits(- number))
      (if(< number 10)
          1
          (+ 1 (count-digits(quotient number 10))))
    )
  )
  (define (to-decimal-iter number pos res dig)
    (if (= pos dig)
        res
        (to-decimal-iter (quotient number 10) (+ pos 1) (+ res (* (remainder number 10) (expt 2 pos))) dig ))
  )
  (to-decimal-iter number 0 0 (count-digits number))
  ;(remainder number 10)
  ;(quotient number 10)
) 
  
;(to-decimal 1010)

(define tests
  (test-suite "to-decimal tests"
    (check-equal? (to-decimal 11001) 25)
    (check-equal? (to-decimal 1100011) 99)
  )
)

(run-tests tests 'verbose)