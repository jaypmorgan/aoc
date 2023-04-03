;; --- Day 5: Supply Stacks ---

;; The expedition can depart as soon as the final supplies have been
;; unloaded from the ships. Supplies are stored in stacks of marked
;; crates, but because the needed supplies are buried under many other
;; crates, the crates need to be rearranged.

;; The ship has a giant cargo crane capable of moving crates between
;; stacks. To ensure none of the crates get crushed or fall over, the
;; crane operator will rearrange them in a series of carefully-planned
;; steps. After the crates are rearranged, the desired crates will be
;; at the top of each stack.

;; The Elves don't want to interrupt the crane operator during this
;; delicate procedure, but they forgot to ask her which crate will end
;; up where, and they want to be ready to unload them as soon as
;; possible so they can embark.

;; They do, however, have a drawing of the starting stacks of crates
;; and the rearrangement procedure (your puzzle input). For example:

;;     [D]    
;; [N] [C]    
;; [Z] [M] [P]
;;  1   2   3 

;; move 1 from 2 to 1
;; move 3 from 1 to 3
;; move 2 from 2 to 1
;; move 1 from 1 to 2

;; In this example, there are three stacks of crates. Stack 1 contains
;; two crates: crate Z is on the bottom, and crate N is on top. Stack
;; 2 contains three crates; from bottom to top, they are crates M, C,
;; and D. Finally, stack 3 contains a single crate, P.

;; Then, the rearrangement procedure is given. In each step of the
;; procedure, a quantity of crates is moved from one stack to a
;; different stack. In the first step of the above rearrangement
;; procedure, one crate is moved from stack 2 to stack 1, resulting in
;; this configuration:

;; [D]        
;; [N] [C]    
;; [Z] [M] [P]
;;  1   2   3 

;; In the second step, three crates are moved from stack 1 to stack
;; 3. Crates are moved one at a time, so the first crate to be moved
;; (D) ends up below the second and third crates:

;;        [Z]
;;        [N]
;;    [C] [D]
;;    [M] [P]
;; 1   2   3

;; Then, both crates are moved from stack 2 to stack 1. Again, because
;; crates are moved one at a time, crate C ends up below crate M:

;;         [Z]
;;         [N]
;; [M]     [D]
;; [C]     [P]
;;  1   2   3

;; Finally, one crate is moved from stack 1 to stack 2:

;;         [Z]
;;         [N]
;;         [D]
;; [C] [M] [P]
;;  1   2   3

;; The Elves just need to know which crate will end up on top of each
;; stack; in this example, the top crates are C in stack 1, M in stack
;; 2, and Z in stack 3, so you should combine these together and give
;; the Elves the message CMZ.

;; After the rearrangement procedure completes, what crate ends up on
;; top of each stack?


;;;------------------------------ Solution -------------------------------

(library (aoc day-five (1))
  (export run)
  (import (rnrs (6))
          (rnrs exceptions))

  (define make-stack
    (lambda args
      (let ([s (apply list args)])
        (define (pop-stack)
          (let ([el (car s)])
            (set! s (cdr s))
            el))
        (define (add-stack el)
          (set! s (cons el s)))
        (define (stack-length) (length s))
        (define (stack-print) s)
        (define (dispatch method . args)
          (cond ((eq? method 'pop) (pop-stack))
                ((eq? method 'add) (apply add-stack args))
                ((eq? method 'print) (stack-print))
                ((eq? method 'length) (stack-length))
                (else (assertion-violation make-stack "Unknown method" method))))
        dispatch)))

  (define (stack-length s) (s 'length))
  (define (stack-add s v) (s 'add v))
  (define (stack-pop s) (s 'pop))
  (define (stack-print s) (s 'print))

  (define stacks (list))

  (define (get-stack idx)
    (list-ref idx stacks))
  
  (define (do-move num from to)
    (cond ((<= num 0) #t)
          (else (begin
                  (stack-add (get-stack to) (stack-pop (get-stack from)))
                  (do-move (- num 1) from to)))))

  (define-syntax move
    (syntax-rules ()
      [(_ el1 from el2 to el3)
       (do-move el1 el2 el3)]))

  (define test-file "2022/data/day5.test.txt")
  (define input-file "2022/data/day5.input.txt")

  ;; string-split: split a string by a delimiter.
  ;; i.e.
  ;; (string-split "this is, a, test" ",") ->
  ;; $1 = ("this is" "a" "test")
  (define (string-split str by)
    ;; split a string by sliding a window of length (string-length by)
    ;; over the str
    (let ([results (list)]              ; start with an empty list we add to
          [window-size (string-length by)]) 
     (define (splitter str pos)
       (cond [(> (+ pos window-size) (string-length str)) (reverse (cons str results))]
             [(string=? (substring str pos (+ pos window-size)) by)
              (begin
                (set! results (cons (substring str 0 pos) results))
                (splitter (substring str (+ pos window-size)) 0))]
             [else (splitter str (+ 1 pos))]))
     (splitter str 0)))

  (define (read-file filepath)
    (with-input-from-file filepath
      (lambda ()
        (let loop ([contents (list)])
          (let ([content (read-char (current-input-port))])
            (if (eof-object? content)
                (list->string (reverse contents))
                (loop (cons content contents))))))))

  (define (read-stack-file filepath)
    (let* ([content   (string-split (read-file filepath) "\n\n")]
           [header    (car content)]
           [movements (filter (lambda (mv) (not (string=? "" mv))) (string-split (cadr content) "\n"))])
      header))

  (define run #f)


  )
