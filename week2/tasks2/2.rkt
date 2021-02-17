#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да проверим дали число е просто.

(define (prime? number)
  (define (prime-iter i)
    (cond
      ((or (= i number) (= number 1)) #t)
      ((and (= (remainder number i) 0) (not (= number i))) #f)
      ((not (= (remainder number i) 0)) (prime-iter (+ i 1)))  
      ))
  (prime-iter 2)
)
(prime? 5)

(define tests
  (test-suite "prime? tests"
    (check-false (prime? 1))
    (check-true (prime? 5))
    (check-false (prime? 1729))
    (check-false (prime? 41041))
  )
)

(run-tests tests 'verbose)
