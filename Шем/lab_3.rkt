;1.
(define-syntax trace
  (syntax-rules()
    ((_ s)
     (begin
       (display 's)
       (display " => ")
       (let ((x s))
         (write x)
         (newline)
         x)))))

(define (zip . xss)
  (if (or (null? xss)
            (null? (trace (car xss)))) 
        '()
        (cons (map car xss)
              (apply zip (map cdr (trace xss))))))
;2.
(define-syntax test
  (syntax-rules ()
    ((_ ex expt)
     (list (quote ex) expt))))

(define (run-test xs) 
  (begin 
    (let ((result (eval (car xs) (interaction-environment))))
      (if (equal? result (cadr xs))
          (begin
            (newline)
            (write (car xs))
            (display "OK \n"))
          (begin
            (write (car xs))
            (display " FAIL")
            (newline)
            (display "\texpected:")
            (write (cadr xs))
            (newline)
            (display "\treturned: ")
            (write result)
            #f)))))
          


(define (run-tests xs)
  (define (loop xs k)
    (if (null? xs)
        k
        (loop (cdr xs) (and k (car xs)))))
  (loop (map run-test xs)  #t))

;3.
(define num 0)
(define (ref xl pos . xss)
  (define (ref1 xs pos)
    (define (loop xs pos num)
      (if (null? xs)
          (display "empty")
          (if (equal? pos num)
              (car xs)
              (if (< num (length xs))
                  (loop (cdr xs) pos (+ num 1))
                  #f))))
    (cond
      ((list? xs)(loop xs pos 0))
      ((vector? xs)(loop (vector->list xs) pos 0))
      ((string? xs)(loop (string->list xs) pos 0))))
  (define (looplist xs ys n pos)
    (if (= pos 0)
        (append (reverse (cons n xs)) ys)
        (looplist (cons (car ys) xs) (cdr ys) n (- pos 1))))
  (if (null? xss)
      (ref1 xl pos)
      (begin
        (set! num (car xss))
        (if (list? xl)
            (and (not (< pos 0))
                 (not  (> pos (length xl)))
                 (looplist '() xl num pos))
            (if (vector? xl)
                (and (not (< pos 0))
                     (not  (> pos (vector-length xl)))
                     (list->vector (looplist '() (vector->list xl) num pos)))
                (if (string? xl)
                    (begin
                      (and (char? num)
                           (and (not (< pos 0))
                                (not (> pos (string-length xl)))
                                (list->string (looplist '() (string->list xl) num pos)))))))))))

;4.
(define (factorize xs)
  (let* ((zn (car xs))
         (first (cadr xs)) (second (caddr xs))
         (x (cadr first)) (y (cadr second))
         (xdeg (caddr first)) (ydeg (caddr second)))
    
    (begin
      (if (and  (equal? '- zn) (equal? xdeg 2) (equal? ydeg 2))
          (list '* (list '- x y) (list '+ x y))
          (if (and (equal? '- zn) (equal? xdeg 3) (equal? ydeg 3))
              (list '* (list '- x y)
                    (list '+ (list 'expt x 2)
                          (list '* x y)
                          (list 'expt y 2)))
              (if (and (equal? '+ zn) (equal? xdeg 3) (equal? ydeg 3))
                  (list '* (list '+ x y)
                        (list '+ (list 'expt x 2)
                              (list '* -1 x y)
                              (list 'expt y 2)))))))))


