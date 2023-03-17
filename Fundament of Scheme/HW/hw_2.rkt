(load "tests.rkt")

;1. Обработка списков
;1.1
(define (my-range a b d) ; O(n) n=b-a
  (if (< a b)
      (cons a (my-range (+ a d) b d))
      '()))

;1.2 
(define (my-flatten xs) ; O(n) n-length xs
  (if (null? xs)
      '()
      (if (pair? xs)
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (list xs))))

;1.3
(define (my-element? x xs) ; O(n) n-length xs
  (if (null? xs)
      #f
      (if (equal? (car xs) x)
          #t
          (my-element? x (cdr xs)))))

;1.4
(define (my-filter pred? xs) ; O(n) n-length xs
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))


;1.5
(define (my-fold-left op xs) ; O(n) n-length xs
  (define (loop x xs)
    (if (null? xs)
        x
        (loop (op x (car xs)) (cdr xs))))
  (loop (car xs) (cdr xs)))

(define (my-fold-right op xs) ; O(n) n-length xs
  (if (null? xs)
      xs
      (if (null? (cdr xs))
          (car xs)
          (op  (car xs) (my-fold-right op (cdr xs))))))

;2. Множества
(define (my-element? x xs) ; O(n) n-length xs
  (if (null? xs)
      #f
      (if (equal? (car xs) x)
          #t
          (my-element? x (cdr xs)))))
;2.1
(define (list->set xs) ; O(n) n-length xs
  (define (loop xs xl)
    (if (null? xs)
        xl
        (if (my-element? (car xs) xl)
            (loop (cdr xs) xl)
            (loop (cdr xs) (cons (car xs) xl)))))
  (loop xs '()))

;2.2
(define (set? xs) ; O(n) n-length xs
  (if (null? xs)
      #t
      (and (not (my-element? (car xs) (cdr xs)))
          (set? (cdr xs)))))


;2.3

(define (union xs ys) ; O(n) n-max length(xs, ys)
  (if (null? xs)
      ys
      (if (my-element? (car xs) ys)
          (union (cdr xs) ys)
          (union (cdr xs) (cons (car xs) ys)))))
;2.4

(define (intersection xs ys) ; O(n) n-max length(xs, ys)
  (define (loop xs ys xll)
    (if (null? xs)
        (reverse xll)
        (if (my-element? (car xs) ys)
            (loop (cdr xs) ys (cons (car xs) xll))
            (loop (cdr xs) ys xll))))
  (loop xs ys '()))


;2.5

(define (difference xs ys) ; O(n) n-max length(xs, ys)
  (define (loop xs ys xll)
    (if (null? xs)
        (reverse xll)
        (if (my-element? (car xs) ys)
            (loop (cdr xs) ys xll)
            (loop (cdr xs) ys (cons (car xs) xll)))))
  (loop xs ys '()))
            

;2.6
(define (symmetric-difference xs ys) ; O(n) n-max length(xs, ys)
  (reverse (append (difference xs ys) (difference ys xs)))) 

;2.7

(define (set-eq? xs ys) ; O(n) n-max length(xs, ys)
   (null? (symmetric-difference xs ys)))



  
;3.1
(define (string-trim-left xs) ; O(n) n-length xs
  (define (loop xs)
    (if (null? xs)
         '()
        (if (char-whitespace? (car xs))
            (loop (cdr xs))
            (list->string (cons (car xs) (cdr xs))))))
  (loop (string->list xs)))
  


(define (string-trim-right xs) ; O(n) n-length xs
   (define (loop xs)
    (if (null? xs)
         '()
        (if (char-whitespace? (car xs))
            (loop (cdr xs))
            (list->string (reverse (cons (car xs) (cdr xs)))))))
  (loop (reverse (string->list xs))))
  


(define (string-trim xs) ; O(2n) n-length xs
    (string-trim-left (string-trim-right xs))) 

;3.2
(define f #t)
(define (string-prefix? a b)  ; O(n) n-length a
  (define (loop a b)
    (and (not (> (length a) (length b)))
         (if (null? a)
            f
            (and (equal? (car a) (car b))
                (loop (cdr a) (cdr b))
                ))))
  (loop (string->list a) (string->list b)))

(define (string-suffix? a b)   ; O(n) n-length a
  (define (loop a b)
    (and (not (> (length a) (length b)))
         (if (null? a)
            f
            (and (equal? (car a) (car b))
                (loop (cdr a) (cdr b))
                ))))
  (loop (reverse (string->list a)) (reverse (string->list b))))



(define (string-infix? a b)  ; O(n) n-length b
  (define (loop a b)
    (if (not (null? a))
        (and (not (null? b))
                (if (equal? (car a) (car b))
                    (loop (cdr a) (cdr b))
                    (loop a (cdr b))))
            f))
  (loop (string->list a) (string->list b)))

;3.3 
(define (string-split str sep) ;O(n^2 * k) k-length sep n-length str
  (define (preobrz str res)
    (if (null? str)
        (reverse res)
        (preobrz (cdr str) (cons (list->string (list (car str))) res))))
  (define (loop str sep xll)
    (if (null? str)
        (begin
          (list->string (reverse xll))
          (if (null? sep)
              (preobrz xll '())
              (loop xll (cdr sep) '())))
        (if (and  (not (null? sep)) (equal? (car str) (car sep)))
            (loop (cdr str) sep xll)
            (loop (cdr str) sep (cons (car str) xll)))))
  (loop (string->list str) (string->list sep) '()))



;4.1
(define (make-multi-vector sizes . fill)
  (define (loop sizes vec)
    (if (null? sizes)
        vec
        (loop (cdr sizes) (vector->list (make-vector (car sizes) vec)))))

(define (list-vect vec xl)
  (if (null? vec)
      xl
      (if (list? (car vec))
          (if (list? (car (car vec)))
              (list-vect (cdr vec) (append xl (list (list->vector (list-vect (car vec) '())))))
              (list-vect (cdr vec) (append xl (list (list->vector (car vec))))))
          (list-vect (cdr vec) xl))))
  (if (null? fill)
      (list->vector (list-vect (loop (reverse sizes) 0) '()))
      (list->vector (list-vect (loop (reverse sizes) (car fill)) '()))))
      
;4.2
(define (multi-vector? m)
  (and (vector? m) (vector? (vector-ref m 0))))

(define (multi-vector-ref m indices)
  (define (loop m ind)
    (if (= (length ind) 1)
        (vector-ref m (car ind))
        (loop (vector-ref m (car ind)) (cdr ind))))
  (loop m indices))

                          
(define (multi-vector-set! m indices x)
  (define (loop m ind x)
    (if (= (length ind) 1)
        (vector-set! m (car ind) x)
        (loop (vector-ref m (car ind)) (cdr ind) x)))
  (loop m indices x))
         




;5
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xl)
  (define (loop x xl)
    (if (null? xl)
        x
        (loop ((car xl) x) (cdr xl))))
  (lambda (x) (loop x (reverse xl))))




(define (list-trim-right xs)
  (define seplist (list '(#\newline 0) '(#\space 0) '(#\tab 0)))
  (define (prov sep)
   (if (assoc sep seplist)
       #t
       #f))
  (define (loop xs xl)
    (if (null? xs)
        xl
        (if (prov (car xs))
            (if (null? (cdr xs))
                xl
                (if (prov (cadr xs))
                    xl
                    (loop (cdr xs) (append xl (list (car xs))))))
            (loop (cdr xs) (append xl (list (car xs)))))))
  (loop xs '()))


  