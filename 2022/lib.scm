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
