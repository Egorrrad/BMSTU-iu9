(define-syntax test
  (syntax-rules ()
    ((_ expr expect)
     (list (quote expr) expect))))

(define (run-test xs) 
  (begin 
    (let ((res (eval (car xs) (interaction-environment))))
      (if (equal? res (cadr xs))
          (begin
            (newline)
            (write (car xs))
            (display "OK \n"))
          (begin
            (newline)
            (write (car xs))
            (display " FAIL \nexpected: ")
            (write (cadr xs))
            (newline)
            (display "returned: ")
            (write res)
            #f)))))
          


(define (run-tests xs)
  (define (loop xs i)
    (if (null? xs)
        i
        (loop (cdr xs) (and i (car xs)))))
  (loop (map run-test xs)  #t))
