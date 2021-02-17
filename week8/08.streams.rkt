#lang racket

(define a '(1 2 3))

(define-syntax my-delay
  (syntax-rules () ((my-delay expr) (lambda () expr))))


(define (my-force p) (p))

(define-syntax stream-cons
  (syntax-rules () ((stream-cons x xs) (cons x (my-delay xs)))))


(define (ones) (stream-cons 1 (ones)))

(define (head s) (car s))
(define (tail s) (my-force (cdr s)))
(define empty-stream '())
(define (stream-empty? s)
  (null? s))

(define (naturals)
  (define (naturals-from x)
    (stream-cons x (naturals-from (+ x 1)))
  )
  (naturals-from 0) 
)

(define (compare-streams s1 s2)
  (cond ((and (stream-empty? s1) (stream-empty? s2)) #t)
        ((or (and (stream-empty? s1) (not (stream-empty? s2)))
             (and (stream-empty? s2) (not (stream-empty? s1)))) #f)
        ((equal? (head s1) (head s2)) (compare-streams (tail s1) (tail s2)))
        (else #f))
)

(define (stream-range s e)
  (if (> s e)
      empty-stream
      (stream-cons s (stream-range (+ s 1) e))))

(define (nth s n)
  (cond
    ((or (stream-empty? s) (< n 0)) #f)
    ((= n 0) (head s))
    (else
     (nth (tail s) (- n 1))))
)

(define (stream-take s n)
  (cond
    ((stream-empty? s) empty-stream)
    ((= n 0) (stream-cons (head s) empty-stream))
    (else
     (stream-cons (head s) (stream-take (tail s) (- n 1)))))
)

(define (stream->list s)
  (cond
    ((stream-empty? s) '())
    (else
     (cons (head s) (stream->list (tail s)))))
)

(define (stream-filter p? s)
  (cond
    ((stream-empty? s) empty-stream)
    ((p? (head s)) (stream-cons (head s) (stream-filter p? (tail s))))
    (else
     (stream-filter p? (tail s))))
)

(define (stream-map f s)
  (cond
    ((stream-empty? s) empty-stream)
    (else
     (stream-cons (f (head s)) (stream-map f (tail s)))))
)

(define (stream-append s t)
  (cond
    ((stream-empty? s) t)
    (else
     (stream-cons (head s) (stream-append (tail s) t))))
)

(define (stream-drop s n)
  (cond
    ((stream-empty? s) empty-stream)
    ((= n 0) s)
    (else
     (stream-drop (tail s) (- n 1))))
)

; правим безкраен поток от вида (x f(x) f(f(x)) f(f(f(x))) ...)
(define (iterate f x)
  (define (iterate-h f nv x)
    (if (stream-empty? x)
        empty-stream
        (stream-cons nv (iterate-h f (f nv) (tail x)))))
  (iterate-h f (head x) (tail x))
)

; правим поток от вида (nv op(nv, a1) op(op(nv, a1), a2) ...)
(define (scanl op nv stream)
  (if (stream-empty? stream)
      empty-stream
      (stream-cons nv (scanl op (op nv (head stream)) (tail stream))))
)
