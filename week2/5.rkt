#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (count-digits number)
  (if(< number 0)
     (count-digits(- number))
     (if(< number 10)
        1
        (+ 1 (count-digits(quotient number 10))))
 ))
(define (reverse-digits number)
  (if ( < number 0)
      (- (reverse-digits (-number)))
      (if (< number 10)
          number
          )

  )
(define (palindrome? number)
  

)