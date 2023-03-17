(load "tests.rkt")

;1.
(define (memoized-factorial n)
  (let ((slovar '()))
    (if (assoc n slovar)
          (cadr (assoc n slovar))
          (let ((res (if (= n 0)
                         1
                         (* n (memoized-factorial (- n 1))))))
            (set! slovar
                  (cons (list n res) slovar))
            res))))
;2.
(define-syntax lazy-cons
  (syntax-rules()
    ((_ a b)(cons a (delay b)))))

(define (lazy-car p)
  (car p))

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
  (define (loop xs k xl)
    (if (= k 0)
        xl
        (loop (lazy-cdr xs) (- k 1) (append xl (list (lazy-car xs))))))
  (loop xs k '()))

(define (lazy-ref xs k)
    (if (= k 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (fact)
  (define (loop x y)
    (lazy-cons (* x y) (loop (+ x 1) (* x y))))
  (loop 1 1))
(define factor (fact))
(define (lazy-factorial n)
  (lazy-ref factor (- n 1)))

;3.
(define (read-words)
  (define (loop xl buf)
    (let ((x (read-char)))
      (if (eof-object? x)
          xl
          (if (char-whitespace? x)
              (if (null? buf)
                  (loop xl '())
                  (loop (append xl (list (list->string (reverse buf)))) '()))
              (loop xl (cons x buf)))
              )))
  (loop '() '()))
  
;4.
(define (define-struct a)
  1)

;5.





(define-syntax my-let
    (syntax-rules ()
      ((my-let ((x e)) body)
       ((lambda (x) body) e))))

(my-let ((x 7))
    (* x x))


