;Задача 1
(define (count x xs)
   (define (loop x xs k)
     (if (null? xs)
         k
         (if (equal? (car xs) x)
             (loop x (cdr xs) (+ k 1))
             (loop x (cdr xs) k))))
   (loop x xs 0))
;Задача 2
(define (delete f xs)
   (if (null? xs)
       '()
       (if (f (car xs))
           (delete f (cdr xs))
           (cons (car xs) (delete f (cdr xs))))))
;Задача 3
(define (iterate f x n)
   (if (= n 0)
       '()
       (cons x (iterate f (f x) (- n 1)))))
;Задача 4
(define (intersperse e xs)
   (if (null? xs)
       '()
       (if (< (length xs) 2)
           (cons (car xs) '())
           (cons (car xs) (cons e (intersperse e (cdr xs)))))))      

;Задача 5
(define (any? f xs)
   (and (not (null? xs)) (or (f (car xs)) (any? f (cdr xs)))))

(define (all? f xs)
   (or (and (not (null? xs)) (and (f (car xs)) (all? f (cdr xs)))) (null? xs)))

;Задача 6
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
   (define (loop x xs)
     (if (null? xs)
         x
         (loop ((car xs) x) (cdr xs))))
   (lambda (x) (loop x (reverse xs))))
