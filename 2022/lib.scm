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

(define (read-file-lines filepath)
  (filter (lambda (str) (not (string= str ""))) (string-split (read-file filepath) #\newline)))

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

(define (slice start end step lst)
  (let ([list-size (length lst)])
   (let loop ([i start] [out (list)])
     (cond ((or (> i end) (>= i list-size)) (reverse out))
	   (else (loop (+ i step) (cons (list-ref lst i) out)))))))

(define (first lst) (car lst))
(define (second lst) (cadr lst))
(define (third lst) (caddr lst))
(define (rest lst) (cdr lst))

(define (sum lst) (apply + lst))


(define (range start stop step)
  (let loop ([out (list)] [i start])
    (cond ((>= i stop) (reverse out))
	  (else (loop (cons i out) (+ i step))))))

;; unique. Given a list find the unique elements of that list.
(define (unique lst)
  (let loop ([it-lst lst] [els (list)])
    (cond [(null? it-lst) (reverse els)]
	  [(member (first it-lst) els) (loop (rest it-lst) els)]
	  [else (loop (rest it-lst) (cons (first it-lst) els))])))

;; union. Find the common elements between two sets.
(define (intersection lst1 lst2)
  (define (f lst1 lst2)
    (cond ((or (null? lst1) (null? lst2)) (list))
	  ((member (car lst1) lst2) (cons (car lst1) (f (cdr lst1) lst2)))
	  (else (f (cdr lst1) lst2))))
  (f lst1 lst2))
