(#%require racket/string)

(define (next x) (+ x 1))
(define (previous x) (- x 1))

(define (is-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (is-op? c) (or (char=? c #\+) (char=? c #\-) (char=? c #\*) (char=? c #\/) (char=? c #\^)))
(define (is-space? c) (char=? c #\space))

(define (expr-valid? expr)
  (define (loop str i len)
    (let
      (
        (curr (get-char str i))
      )
      (cond
        ((not (or (is-digit? curr) (is-op? curr) (is-space? curr))) #f)
        ((and (= i 0) (is-op? curr)) #f)
        ((and (= i (- len 1)) (not (is-digit? curr))) #f)
        ((= i (- len 1)) #t)
        ((and (is-op? curr) (is-op? (string-ref str (next i)))) #f)
        ((and (is-space? curr) (is-digit? (string-ref str (previous i))) (is-digit? (string-ref str (next i)))) #f)
        ((and (is-space? curr) (is-op? (string-ref str (previous i))) (is-op? (string-ref str (next i)))) #f)
        (else (loop str (next i) len))
      )
    )
  )

  (define normal-expr (string-normalize-spaces expr))
  (if (= (string-length normal-expr) 0)
     #t
     (loop normal-expr 0 (string-length normal-expr))
  )
)

(define (get-char str i)
  (if (or (= (string-length str) 0) (< i 0) (>= i (string-length str)))
      #\null
      (string-ref str i))
)

(define (get-elem str i)
  (if (or (= (string-length str) 0) (< i 0) (>= i (string-length str)))
      ""
      (string (get-char str i)))
)

(define (replace str from to)
   (if (= (string-length str) 0)
      ""
      (string-replace str str (substring str from to))) 
)

(define (cut-first str)
  (replace str 1 (string-length str))
)

(define (cut-last str)
  (replace str 0 (- (string-length str) 1))
)

(define (take-op c)
  (cond
    ((or (char=? c #\+) (char=? c #\-)) 0)
    ((or (char=? c #\*) (char=? c #\/)) 1)
    (else 2)
  )
)

(define (expr-rp expr)
  (define (invert str i length help result c)
    (cond
      ((and (= i length) (not (eq? help ""))) (invert str i length (cut-last help) (string-append result (get-elem help (- (string-length help) 1))) ""))
      ((= i length) result)
      ((and (is-op? (get-char str i)) (eq? help ""))
       (invert str (next i) length (string-append help (get-elem str i)) (string-append result c) c))
      ((and (is-op? (get-char str i)) (<= (take-op (get-char str i)) (take-op (get-char help (- (string-length help) 1)))))
       (invert str i length (cut-last help) (string-append result (get-elem help (- (string-length help) 1))) ""))
      ((and (is-op? (get-char str i)) (> (take-op (get-char str i)) (take-op (get-char help (- (string-length help) 1)))))
       (invert str (next i) length (string-append help (get-elem str i)) (string-append result c) c)) 
      (else (invert str (next i) length help (string-append result (get-elem str i)) ","))
    )
  )
  
  (define original (string-replace expr " " ""))
  (cond
    ((not (expr-valid? expr)) #f)
    ((= (string-length original) 0) "")
    (else (invert original 0 (string-length original) "" "" ""))
  )
)

(define (operate op a b)
  (cond
    ((char=? op #\+) (+ a b))
    ((char=? op #\-) (- a b))
    ((char=? op #\*) (* a b))
    ((char=? op #\/) (/ a b))
    ((char=? op #\^) (expt a b))
    (else 0)
  )
)

(define (cut-number str i n result)
   (cond
     ((and (char? result) (or (char=? (get-char str (- (string-length str) 1)) #\,) (= (string-length str) 0)) (cut-last str)))
     ((or (char=? (get-char str (- (string-length str) 1)) #\,) (= (string-length str) 0)) (string->number result))
     ((char? result) (cut-number (cut-last str) (previous i) n result))
     (else (cut-number (cut-last str) (previous i) n (string-append (get-elem str (- (string-length str) 1)) result)))
    )
)

(define (pop-number str)
  (if (= (string-length str) 0)
      ""
      (cut-number str (previous (string-length str)) 0 #\null))
)

(define (double-pop-number str)
  (pop-number (pop-number str))
)

(define (take-last str)
  (if (= (string-length str) 0)
      0
      (cut-number str (previous (string-length str)) 0 ""))
)

(define (take-before str)
  (if (= (string-length str) 0)
      0
      (take-last (pop-number str))
  )
)

(define (add str number)
  (if (= (string-length str) 0)
    (string-append str (number->string number))
    (string-append str "," (number->string number))
  )
)

(define (expr-eval expr)
  (define (calculate str i length save-str sym)
      (cond
        ((= i (- length 1)) (operate (get-char str i) (take-before save-str) (take-last save-str)))
        ((or (is-digit? (get-char str i)) (char=? (get-char str i) #\,)) (calculate str (next i) length (string-append save-str sym (get-elem str i)) ""))
        (else (calculate str (next i) length (add (double-pop-number save-str) (operate (get-char str i) (take-before save-str) (take-last save-str))) ","))
      )   
  )

  (cond
    ((not (expr-valid? expr)) #f)
    ((eq? (expr-rp expr) "") 0)
    (else (calculate (expr-rp expr) 0 (string-length (expr-rp expr)) "" ""))
  )
)

