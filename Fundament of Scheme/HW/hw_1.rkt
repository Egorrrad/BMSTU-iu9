;Задача 1
(define (day-of-week d m y)
   (if(< m 3)
      (remainder (+ 1 (remainder(+ d 3 (- y 1) (quotient (- y 1) 4) (-(quotient (- y 1) 100)) (quotient (- y 1) 400) (quotient(+ (* 31 m) 10) 12)) 7) ) 7)
      (remainder (+ 1 (remainder(+ d y (quotient y 4) (-(quotient y 100)) (quotient y 400) (quotient(+ (* 31 m) 10) 12)) 7) )   7)))

;Задача 2
(define (square-equation a b c)
      (define d (- (* b b) (* 4 a c)))
      (cond ((< d 0) '())
            ((= a b 0) "endless solution")
            ((= d 0) (list (/(* (- 1) b) (* 2 a))))
            ((= a 0) (list (- (/ c b))))
            ((> d 0) (cons (/ (- (- b) (sqrt d)) (* a 2)) (list (/ (- (sqrt d) b) (* a 2)))))))


;Задача 3
;3.1
(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))

;3.2
(define (my-lcm a b)
   (/ (abs (* a b))(my-gcd a b)))

;3.3
(define (prime? a)
  (define (prime2 a b)
  (or (< a (* 2 b)) (and (not (= (remainder a b) 0)) (prime2 a (+ b 1)))))
  (if (= a 1)
      #t
       (prime2 (abs a) 2)))
