#lang racket

(require rackunit)
(require rackunit/text-ui)

(define (count-digits number sum)
  (if(= number 0)
     sum
     (if(< number 0)
        (count-digit (- number) sum))
     

)