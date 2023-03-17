(load "tests.rkt")
;<expression> ::= <znak><number> | <number> | <sep><expression> | <expression><sep> | <empty>
;<number> ::= <nums>/<nums>
;<nums> :: =<num><nums> | <num>
;<znak> ::= + | - | <empty>
;<sep> ::= space | newline | tab  
;<num> ::= 0|1|2|3|4|5|6|7|8|9

;1.

(define (check-frac str)
  (define (prov num)
    (char<=? #\0 num #\9))
  (define (loop str count zn res)
    (if (null? str)
        (= res 1)
        (if (or (> count 1) (> zn 1))
            #f
            (if (or (char=? (car str) #\+) (char=? (car str) #\-))
                (if (null? (cdr str))
                    #f
                    (if (prov (cadr str))
                        (loop (cdr str) count (+ zn 1) 1)
                        #f))
                (if (prov (car str))
                    (loop (cdr str) count zn 1)
                    (if (equal? (car str) #\/)
                        (loop (cdr str) (+ count 1) zn 1)
                        #f))))))
  (if (< (length (string->list str)) 3)
        #f
        (loop (string->list str) 0 0 0)))
                

(define (scan-frac str)
  (define str1 str)
  (if (check-frac str)
      (string->number str1)
      #f))

(define (scan-many-fracs str)
  (define (raskrnum str)
    (define (loop str str1)
      (if (null? str)
        str1
        (let ((x (string->number (car str))))
          (if x
              (loop (cdr str) (cons (string->number (car str)) str1))
              #f))))
    (loop str '()))
  (define (loop str str1 p)
    (if (null? str)
        (begin
          (reverse str1)
          (raskrnum str1))
        (if (or (char=? (car str) #\tab) (char=? (car str) #\space) (char=? (car str) #\newline))
            (if (null? p)
                (loop (cdr str) str1 '())
                (loop (cdr str) (cons (list->string (reverse p)) str1) '()))
            (loop (cdr str) str1 (cons (car str) p)))))
  (loop (append (string->list str) (list #\space)) '() '()))


;2
(define call-cc call-with-current-continuation)

;; Конструктор потока
(define (make-stream elements . end-of-string)
  (if (null? end-of-string)
      (make-stream elements #f)
      (list elements (car end-of-string))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | пустота.
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | пустота .

(define (parse input-vec)
      
  (let* ((end-of-prog (integer->char 0))
         (stream (make-stream (vector->list input-vec) end-of-prog)))

    (define (program stream error);; разбор программы по ъграмматика
      (cond
        ((equal? end-of-prog (peek stream))
         (error #f))
        (else
         (list(articles stream error) (body stream error)))))

    (define (articles stream error)
      (cond
        ((equal? (peek stream) 'define)
         (next stream)  ;съедаем define =)
         (cons (article stream error) (articles stream error)))
        (else '())))
             
    (define (article stream error)
      (cond
        ((word? (peek stream))
         (cons (next stream) ; съедаем word =)
               (cons (body stream error) ; съедаем body =)
                     (if (equal? 'end (peek stream))
                         (begin
                           (next stream) ; съедаем end =)
                           '())
                         (error #f)))))
        (else (error #f))))

    (define (body stream error)
      (cond
        ((equal? 'if (peek stream))
         (next stream)
         (cons (cons 'if
                     (cons (body stream error)
                           (if (and (equal? 'endif (peek stream)) (next stream))
                               '()
                               (error #f))))
               (body stream error)))
        ((number? (peek stream))
         (cons (next stream) (body stream error)))
        ((word? (peek stream))
         (cons (next stream) (body stream error)))
        (else '())))
            
    (define (word? x)
      (and (not (member x '(define end if endif))) (symbol? x)))
    
    (if (null? (vector->list input-vec))
        '(()())
        (call-cc
         (lambda (error)
           (define result (program stream error))
           (and (equal? (peek stream) end-of-prog) result))))))


