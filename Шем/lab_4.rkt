;1.
(define call/cc call-with-current-continuation)
(define f #f)

(define (use-assertions)
   (call/cc
          (lambda (exit)
            (set! f (lambda (x)
                      (display "FAILED: ")
                      (write x)
                      (exit))))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr) (if (not expr)
                  (f 'expr)))))


;2.
(define (save-data data way)
  (call-with-output-file way (lambda (p)
                               (write data p))))

(define (load-data way)
  (call-with-input-file way (lambda (p)
                               (read p))))
(define (strok way)
  (define (loop count way)
    (if (eof-object? (peek-char way))
        count
        (if (equal? #\newline (read-char way))
            (loop (+ count 1) way)
            (loop count way))))
  (call-with-input-file way
    (lambda (w)
      (loop 1 w))))

          
;3.  
(define (trib n)
  (cond
    ((<= n 1) 0)
    ((= n 2) 1)
    ((> n 2) (+ (trib(- n 1)) (trib(- n 2)) (trib(- n 3))))))

(define (trib-mem n)
  (define (loop known-res n)
    (if (assoc n known-res)
        (cadr (assoc n known-res))
        (let ((res (cond
                     ((or (= n 1) (= n 0)) 0)
                     ((= n 2) 1)
                     (else (+ (trib-mem (- n 1)) (trib-mem (- n 2)) (trib-mem (- n 3)))))))
          (set! known-res (cons (list n res) known-res))
          res)))
  (loop '() n))

;4.
(define-syntax my-if
  (syntax-rules()
    ((_ cond? a b)(force (or (and cond? (delay a)) (delay b))))))


;5 
(define-syntax my-let
  (syntax-rules ()
    ((_ ((nam val) ...) expr1 . expr2)
     ((lambda (nam ...) expr1 . expr2)
      val ...))))

(define-syntax my-let*
  (syntax-rules ()
  ((_ ((nam val)) expr1 ...) ((lambda (nam) expr1 ...) val))
  ((_ ((nam val) . nams) expr1 ...) ((lambda (nam) (my-let* nams expr1 ...)) val))))
;6.A
(define x 1)
(define-syntax when
  (syntax-rules ()
    ((_ cond? x . xl)
     (if cond?
         (begin x . xl)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond? x . xl)
     (if (not cond?)
         (begin x . xl)))))

;6.Б
(define-syntax for
  (syntax-rules (as in)
    ((_ x in xs . xl) (for-each (lambda (x) (begin . xl)) xs))
    ((_ xs as x . xl) (for-each (lambda (x) (begin . xl)) xs))))


;6.В
(define-syntax while
  (syntax-rules ()
    ((_ cond? . x)
     (let end () (when cond? (begin . x) (end))))))

;6.Г
(define-syntax repeat
  (syntax-rules (until)
    ((_ (x . xs) until cond?)
     (let end ()
       (begin x . xs)
       (if (not cond?) (end))))))

;6.Д 
(define-syntax cout
  (syntax-rules (<< endl)
    ((_ << x) (display x))
    ((_ << x . xs)(begin (display x)
                          (cout . xs)))
    ((_ << endl . xs) (begin (newline)
                             (cout . xs)))
    ((_ << endl) (newline))))
           


