
(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (transform set from to)
  (define (loop set i result)
    (if (= set 0)
       result
       (loop (quotient set to) (1+ i) (+ result (* (remainder set to) (expt from i)))))
  )
  (loop set 0 0)
)

(define (binary set) (transform set 10 2))
(define (decimal set) (transform set 2 10))

(define (mutate op set elem)
  (decimal (op (binary set) (expt 10 elem)))
)

(define (set-add set elem)
  (if (set-contains? set elem)
      set
      (mutate + set elem))
)

(define (set-remove set elem)
  (if (not (set-contains? set elem))
      set
      (mutate - set elem))
)

(define (set-contains? set elem)
  (define (search set i elem)
    (let
     (
       (rem (remainder set 10))
     )
     (cond
       ((and (= i elem) (= rem 1)) #t)
       ((and (= i elem) (= rem 0)) #f)
       (else (search (quotient set 10) (1+ i) elem))
     )
    )
  )
  
  (if (set-empty? set)
     #f
     (search (binary set) 0 elem))
)

(define (set-empty? set)
  (if (= set 0)
     #t
     #f)
)

(define (set-size set)
  (define (count set len)
    (let
       (
         (rem (remainder set 10))
       )
       (cond
         ((set-empty? set) len)
         ((= rem 0) (count (quotient set 10) len))
         (else (count (quotient set 10) (1+ len)))    
       )
    ) 
  )
  (count (binary set) 0)
)

(define (set-full-length set)
  (if (set-empty? set)
      0
      (string-length (number->string (binary set))))
)

(define (combine set1 set2 i set-result operation p? value1 value2)
    (let                                 
      (
        (rem (remainder set1 10))
        (s2-len (set-full-length set2))
      )
      (cond
        ((set-empty? set1) set-result)
        ((and (>= i s2-len) (= rem 1)) (combine (quotient set1 10) set2 (1+ i) (set-add set-result i) operation p? value1 value2))
        ((and (>= i s2-len) (= rem 0)) (combine (quotient set1 10) set2 (1+ i) set-result operation p? value1 value2))
        ((= rem value1) (combine (quotient set1 10) set2 (1+ i) (operation set-result i) operation p? value1 value2))
        ((and (= rem value2) (p? set2 i)) (combine (quotient set1 10) set2 (1+ i) set-result operation p? value1 value2))
        (else (combine (quotient set1 10) set2 (1+ i) (set-add set-result i) operation p? value1 value2))
      )
    )
)

(define (set-intersect s1 s2)
  (define (id x y) x)
  (define (no-contains? set elem) (not (set-contains? set elem)))
  (cond
    ((or (set-empty? s1) (set-empty? s2)) 0)
    ((= s1 s2) s1)
    ((> s1 s2) (combine (binary s2) s1 0 0 id no-contains? 0 1))
    (else (combine (binary s1) s2 0 0 id no-contains? 0 1))
  )
)

(define (set-union s1 s2)
  (define (add set elem) (set-add set elem))
  (define (no-contains? set elem) (not (set-contains? set elem)))
  (cond
    ((and (set-empty? s1) (set-empty? s2)) 0)
    ((set-empty? s1) s2)
    ((set-empty? s2) s1)
    ((= s1 s2) s1)
    ((< s1 s2) (combine (binary s2) s1 0 0 add no-contains? 1 0))
    (else (combine (binary s1) s2 0 0 add no-contains? 1 0))
  )
)

(define (set-difference s1 s2)
  (define (id x y) x)
  (cond
    ((or (set-empty? s1) (= s1 s2)) 0)
    ((set-empty? s2) s1)
    (else (combine (binary s1) s2 0 0 id set-contains? 0 1))
  )
)

(define (calculate set p)
  (define (loop set i price)
    (let
      (
        (rem (remainder set 10))
      )
      (cond
        ((= set 0) price)
        ((= rem 1) (loop (quotient set 10) (1+ i) (+ price (p i))))
        (else (loop (quotient set 10) (1+ i) price))
      )
    )
  )
  (loop (binary set) 0 0)
)

(define (knapsack c n w p)
  (define (take c n w p set-result)
    (let
      (
        (curr-i (1- n))
      )
      (cond
        ((or (= n 0) (= c 0)) set-result)
        ((> (w curr-i) c) (take c curr-i w p set-result))
        (else
          (let (
                (first (+ (p curr-i) (calculate (take (- c (w curr-i)) curr-i w p (set-add set-result curr-i)) p)))
                (second (calculate (take c curr-i w p set-result) p))
               )
               (if (> first second)
                 (take (- c (w curr-i)) curr-i w p (set-add set-result curr-i))
                 (take c curr-i w p set-result)
               )
           )
         )
       )
    ) 
  )

  (take c n w p 0)
)

