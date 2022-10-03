#lang racket
(require parser-tools/lex)

;;Scanner/Lexer, creates tokens matching the trigger and forms a list of tokens
(define calclexer
  (lexer
   ;ignore white space
   [whitespace (calclexer input-port)]

   ;read
   ["read" (cons `("RE" ,lexeme)(calclexer input-port))]

   ;write
   ["write" (cons `("WRI" ,lexeme)(calclexer input-port))]

   ;IDS / letters
   [(repetition 1 +inf.0 (union(char-range #\a #\z)(char-range #\A #\Z))) (cons `("ID" ,lexeme)(calclexer input-port))]

   ;assignment :=
   [":=" (cons `("ASN" ,lexeme)(calclexer input-port))]
   
   ;numbers
   [(repetition 1 +inf.0 (char-range #\0 #\9)) (cons `("NUM" ,lexeme)(calclexer input-port))]

   ;+ or -
   [(union #\+ #\-) (cons `("A_OP" ,lexeme)(calclexer input-port))]

   ;/ or *
   [(union #\/ #\*) (cons `("M_OP" ,lexeme)(calclexer input-port))]

   ;L paren
   [#\( (cons `("Lparen" ,lexeme)(calclexer input-port))]

   ;R paren
   [#\) (cons `("Rparen" ,lexeme)(calclexer input-port))]

   ;$$
   ["$$" (cons `("EOF" ,lexeme)(calclexer input-port))]
   [(eof)(void)]
   ))

;;Takes off the void from lexer and calls parse-program on rest of tokens, print "Accept" or "Error" based on result
(define (parse input)
  (if(equal? #t (parse-program (drop-right(calclexer(open-input-file input))0)))(display "Accept")(display "Error"))
  )

(define (parse-program input)
  (eof_(stmt_list input))
  )

(define (stmt_list input)
  (cond
    [(equal? #f input) #f]
    [(empty? input) input]
    [(equal? "EOF" (caar input)) input]
    [(stmt_list(stmt input))]
    [else #f])
  )

(define (stmt input)
  (cond
    [(id_stmt input)]
    [(read_stmt input)]
    [(write_stmt input)]
    [else #f])
  )

;;DEFINES THE FORM OF 

(define (id_stmt input)
  (expr (asn_op (id input)))
  )

(define (read_stmt input)
   (id (read_ input))
  )

(define (write_stmt input)
   (expr (write_ input))
  )

;; EXPRESSION

(define (expr input)
  (cond
    [(equal? #f input) #f #f]
    [(term_tail (term input))] 
    [else #f])
  )

;; TERM / FACTOR and TERM_TAIL / FACTOR_TAIL

(define (term input)
  (cond
    [(equal? #f input) #f #f]
    [(factor_tail (factor input))] 
    [else #f])
  )
 
(define (term_tail input)
  (cond
    [(equal? #f input) #f]
    [(empty? input) #f]
    [(equal? #f (add_op input)) input]
    [(term_tail (term (add_op input)))]
    [else #f])
  )
 
(define (factor input)
  (cond
   [(right_p (expr (left_p input)))]
   [(id input)]
   [(equal? (number_ input) (drop input 1)) (number_ input)]
   [else #f])
  )

(define (factor_tail input)
  (cond
    [(equal? #f input) #f]
    [(empty? input) #f]
    [(equal? #f (mult_op input)) input]
    [(factor_tail (factor (mult_op input)))]
    [else #f])
  )

;; TERMINALS ;; BOTTOM OF TREE ;; ALL FUNCTIONS HERE REMOVE A TOKEN (barring EOF) ;;

(define (mult_op input)
  (cond
    [(equal? #t input) input]
    [(equal? "M_OP" (caar input)) (drop input 1)]
    [(equal? "M_OP" (caar input)) (drop input 1)]
    [else #f]))

(define (add_op input)
  (cond
    [(equal? #t input) input]
    [(equal? "A_OP" (caar input)) (drop input 1)]
    [(equal? "A_OP" (caar input)) (drop input 1)]
    [else #f]))

(define (id input)
  (cond
    [(equal? #f input) #f #f]
    [(equal? "ID" (caar input))(drop input 1)]
    [else #f])
  )

(define (read_ input)
  (cond
    [(equal? #f input) #f #f]
    [(equal? "RE" (caar input))(drop input 1)]
    [else #f])
  )

(define (write_ input)
  (cond
    [(equal? #f input) #f #f]
    [(equal? "WRI" (caar input))(drop input 1)]
    [else #f])
  )

(define (asn_op input)
  (cond
    [(equal? #f input) #f #f]
    [(equal? "ASN" (caar input))(drop input 1)]
    [else #f])
  )

(define (number_ input)
  (cond
    [(equal? #f input) #f #f]
    [(equal? "NUM" (caar input))(drop input 1)]
    [else #f])
  )

(define (right_p input)
  (cond
    [(equal? #f input) #f]
    [(equal? "Rparen" (caar input))(drop input 1)]
    [else #f])
  )

(define (left_p input)
  (cond
    [(equal? #f input) #f]
    [(equal? "Lparen" (caar input))(drop input 1)]
    [else #f])
  )

(define (eof_ input)
  (cond
    [(empty? input) #f]
    [(equal? #f input) #f #f]
    [(equal? "EOF" (caar input))#t]
    [else #f])
  )

(parse "input01.txt")
