;; read a file into a string
;; read-file : filepath -> str
(define (read-file filepath)
  (with-input-from-file filepath
    (lambda ()
      (let loop ([contents (list)])
	(let ([current-line (read-char (current-input-port))])
	  (if (eof-object? current-line)
	      (list->string (reverse contents))
	      (loop (cons current-line contents))))))))

;; Take N elements from LST.
;; i.e.
;; (take 3 '(a b c d e f)) => '(a b c)
;; (take 3 '()) => '()
;; (take 3 '(a)) => '(a)
;; take : int list -> list
(define (take n lst)
  (define (taker i out lst)
    (if (or (null? lst) (>= i n))
	(reverse out)
	(taker (+ i 1) (cons (car lst) out) (cdr lst))))
  (taker 0 (list) lst))

(define (first lst) (car lst))
(define (second lst) (cadr lst))
(define (third lst) (caddr lst))
(define (rest lst) (cdr lst))

(define (sum lst) (apply + lst))
