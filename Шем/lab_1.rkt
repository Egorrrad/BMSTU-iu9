(define (my-odd a)
  (if (= (remainder a 2)0)
      #t
  (if (= (remainder a 2)1)
      #f)))

(define (my-even a)
  (if (= (remainder a 2)0)
      #f
  (if (= (remainder a 2)1)
      #t)))

(define (power b e)
  (if (= e 0)
      1
      (* b (power b (- e 1)))))
