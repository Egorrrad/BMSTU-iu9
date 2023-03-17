(load "tests.rkt")

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

;<str> ::= <symbol><word><symbol> | <word> | <sep><str> | <sep><number> | empty
;<symbol> ::= "(" | ")" | + | - | * | | / | ^
;<word> ::= <letter><word> | empty
;<letter> ::= a | b | c | ... | A | B | C ...
;<number> ::= <num><number>
;<num> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;<sep> ::= #\space

;1. Лексический анализатор
(define (tokenize str)
    (define (symbol? x)
      (member x '( #\+ #\- #\/ #\* #\^)))
    (define (tok str xl)
      (cond
        ((null? str)
         xl)
        ((char-whitespace? (car str))
         (tok (cdr str) xl))
        
        ((member (car str) '(#\( #\)))
         (tok (cdr str) (append xl (list (list->string (list (car str)))))))
        
        ((symbol? (car str))
          (tok (cdr str) (append xl (list (string->symbol (list->string (list (car str))))))))
        
        ((char-numeric? (car str))
         (tok (cdr str) (append xl (list (string->number (list->string (list (car str))))))))
        
        ((char-alphabetic? (car str))
         (if (and (not (null? (cdr str))) (char-alphabetic? (cadr str)))
             (tok (cddr str)
                  (append xl (list (string->symbol (list->string (list (car str) (cadr str)))))))
             (tok (cdr str)
                  (append xl (list (string->symbol (list->string (list (car str)))))))))
        (else #f)))
    (tok (string->list str) '()))

;2. Синтаксический анализатор

#|
Expr    ::= Term Expr' .
Expr'   ::= AddOp Term Expr' | .
Term    ::= Factor Term' .
Term'   ::= MulOp Factor Term' | .
Factor  ::= Power Factor' .
Factor' ::= PowOp Power Factor' | .
Power   ::= value | "(" Expr ")" | unaryMinus Power .
|#

(define (parse lis)
  (let* ((end (integer->char 0))
         (stream (make-stream lis end)))
    (define (expr stream error)
      (let loop ((flag (term stream error)))
        (if (member (peek stream) '(+ -))
             (loop (list flag (next stream) (term stream error)))
             flag)))
  
    (define (term stream error)
      (let loop ((flag (factor stream error)))
        (if (member (peek stream) '(* /))
            (loop (list flag (next stream) (factor stream error)))
             flag)))
    
    (define (factor stream error)
      (let loop ((flag (power stream error)))
        (if (equal? (peek stream) '^)
            (loop (list flag (next stream) (factor stream error)))
            flag)))

    (define (power stream error)
      (cond ((equal? '- (peek stream))
             (list (next stream) (power stream error)))
            ((number? (peek stream))
             (next stream))
            ((symbol? (peek stream))
             (next stream))
            ((equal? "(" (peek stream))
             (next stream)
             (let ((flag (expr stream error)))
               (if (and (equal? ")" (peek stream)) (next stream))
                   flag
                   (error #f))))
            (else (error #f))))
    
    (call-cc
     (lambda (error)
       (define result (expr stream error))
       (and (equal? (peek stream) end)
            result)))))

;3. Преобразователь дерева разбора в выражение на Scheme

(define (tree->scheme par)
  (if (and (list? par) (= (length par) 3))
      (if (equal? (cadr par) '^)
          (list 'expt (tree->scheme (car par)) (tree->scheme (caddr par)))
          (list (cadr par) (tree->scheme (car par)) (tree->scheme (caddr par))))
      par))

