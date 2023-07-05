;; --- Day 7: No Space Left On Device ---

;; You can hear birds chirping and raindrops hitting leaves as the
;; expedition proceeds. Occasionally, you can even hear much louder
;; sounds in the distance; how big do the animals get out here,
;; anyway?

;; The device the Elves gave you has problems with more than just its
;; communication system. You try to run a system update:

;; $ system-update --please --pretty-please-with-sugar-on-top
;; Error: No space left on device

;; Perhaps you can delete some files to make space for the update?

;; You browse around the filesystem to assess the situation and save
;; the resulting terminal output (your puzzle input). For example:

;; $ cd /
;; $ ls
;; dir a
;; 14848514 b.txt
;; 8504156 c.dat
;; dir d
;; $ cd a
;; $ ls
;; dir e
;; 29116 f
;; 2557 g
;; 62596 h.lst
;; $ cd e
;; $ ls
;; 584 i
;; $ cd ..
;; $ cd ..
;; $ cd d
;; $ ls
;; 4060174 j
;; 8033020 d.log
;; 5626152 d.ext
;; 7214296 k

;; The filesystem consists of a tree of files (plain data) and
;; directories (which can contain other directories or files). The
;; outermost directory is called /. You can navigate around the
;; filesystem, moving into or out of directories and listing the
;; contents of the directory you're currently in.

;; Within the terminal output, lines that begin with $ are commands
;; you executed, very much like some modern computers:

;; - cd means change directory. This changes which directory is the
;; current directory, but the specific result depends on the argument:
;; - cd x moves in one level: it looks in the current directory for
;; the directory named x and makes it the current directory.
;; - cd .. moves out one level: it finds the directory that contains
;; the current directory, then makes that directory the current
;; directory.
;; - cd / switches the current directory to the outermost directory,
;; /.
;; - ls means list. It prints out all of the files and directories
;; immediately contained by the current directory:
;; - 123 abc means that the current directory contains a file named
;; abc with size 123.
;; - dir xyz means that the current directory contains a directory
;; named xyz.
;; - Given the commands and output in the example above, you can
;; determine that the filesystem looks visually like this:

;; - / (dir)
;;   - a (dir)
;;     - e (dir)
;;       - i (file, size=584)
;;     - f (file, size=29116)
;;     - g (file, size=2557)
;;     - h.lst (file, size=62596)
;;   - b.txt (file, size=14848514)
;;   - c.dat (file, size=8504156)
;;   - d (dir)
;;     - j (file, size=4060174)
;;     - d.log (file, size=8033020)
;;     - d.ext (file, size=5626152)
;;     - k (file, size=7214296)

;; Here, there are four directories: / (the outermost directory), a
;; and d (which are in /), and e (which is in a). These directories
;; also contain files of various sizes.

;; Since the disk is full, your first step should probably be to find
;; directories that are good candidates for deletion. To do this, you
;; need to determine the total size of each directory. The total size
;; of a directory is the sum of the sizes of the files it contains,
;; directly or indirectly. (Directories themselves do not count as
;; having any intrinsic size.)

;; The total sizes of the directories above can be found as follows:

;; - The total size of directory e is 584 because it contains a single
;; file i of size 584 and no other directories.
;; - The directory a has total size 94853 because it contains files f
;; (size 29116), g (size 2557), and h.lst (size 62596), plus file i
;; indirectly (a contains e which contains i).
;; - Directory d has total size 24933642.
;; - As the outermost directory, / contains every file. Its total size
;; is 48381165, the sum of the size of every file.
;; - To begin, find all of the directories with a total size of at
;; most 100000, then calculate the sum of their total sizes. In the
;; example above, these directories are a and e; the sum of their
;; total sizes is 95437 (94853 + 584). (As in this example, this
;; process can count files more than once!)
;; - Find all of the directories with a total size of at most
;; 100000. What is the sum of the total sizes of those directories?

;;;------------------------------ Solution -------------------------------

(use-modules (ice-9 rdelim))

(load "2022/lib.scm")

(define (dir-make name files) `(,name ,files))
(define dir-name car)
(define dir-files cadr)

(define (file-make name size) `(,name . ,size))
(define file-name car)
(define file-size cdr)

(define dir? list?)
(define file? pair?)

(define (dir-list dir)
  (let loop ((contents (dir-files dir)))
    (cond ((null? contents) (format (current-output-port) "~&"))
          ((dir? (car contents))
           (begin
             (format (current-output-port) "dir ~A~&" (dir-name (car contents)))
             (loop (cdr contents))))
          (else (begin
                  (format (current-output-port)
                          "~A ~A~&"
                          (file-size (car contents))
                          (file-name (car contents)))
                  (loop (cdr contents)))))))


(define (add-node! node item) (set-cdr! node  (cons item (dir-files node))))

(define (find-folder name dirs)
  (cond ((null? dirs) #f)
        ((string= name (dir-name (car dirs))) (car dirs))
        (else (find-folder name (cdr dirs)))))

(define (but-last lst)
  (reverse (cdr (reverse lst))))

(define (add-to-end lst item)
  (append lst (list item)))

(define (make-system)
  ;; make a class like structure
  (let ((system-files (dir-make "/" (list)))
        (wd (list)))
    
    ;; iterate through the pwd to get the current directory path
    (define (get-path)
      (let loop ((contents system-files)
                 (wd wd))
        (cond ((null? wd) contents)
              (else (loop (find-folder (car wd) (dir-files contents)) (cdr wd))))))

    ;; change directory by adding the name onto the pwd list, unless
    ;; the name is ".." in which case we will pop this directory off
    ;; the pwd list.
    (define (cd name)
      (cond ((string= ".." name) (set! wd (but-last wd)))
            ((string= "/"  name) (set! wd (list)))
            (else (set! wd (add-to-end wd name)))))

    (define (add-file dir file)
      (set-car! (list-tail dir 1) (add-to-end (dir-files dir) file)))

    ;; print the current directory
    (define (ls) (dir-list (get-path)))
    (define (mkdir name) (add-file (get-path) (dir-make name (list))))
    (define (touch filename size) (add-file (get-path) (file-make filename size)))
    (define (pwd) wd)
    (define (files) system-files)

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'cd) (apply cd args))
            ((eq? cmd 'mkdir) (apply mkdir args))
            ((eq? cmd 'touch) (apply touch args))
            ((eq? cmd 'pwd) (pwd))
            ((eq? cmd 'files) (files))
            ((eq? cmd 'ls) (ls))))      
    dispatch))

(define system-files (make-system))


(define test-file "2022/data/day7.test.txt")

;; does the string start with a predicate
;; example
;; (string-starts-with? "this is a string", #\t) => #t
(define (string-starts-with? str char-pred)
  (char=? (string-ref str 0) char-pred))

;; is the terminal output line a command?
;; example:
;; $ ls => #t
;; dir a => #f
(define (cmd? str)
  (string-starts-with? str #\$))

(define (term-dir? str)
  (string= "dir" (substring str 0 3)))

(define (term-file? str)
  (not (term-dir? str)))

(define (term-type str)
  (cond ((cmd? str) 'cmd)
        ((term-dir? str) 'dir)
        ((term-file? str) 'file)))

(define (term-format str)
  (let ((type (term-type str)))

    (define (format-cmd s)
      (let ((data (cdr (string-split s #\space))))
        `(,(string->symbol (car data)) ,@(cdr data))))

    (define (format-dir s) (string-split s #\space))

    (define (format-file s)
      (let ((data (string-split s #\space)))
        (list (string->number (first data)) (second data))))
    
    (cond ((eq? type 'cmd) (format-cmd str))
          ((eq? type 'dir) (format-dir str))
          ((eq? type 'file) (format-file str)))))

(define (read-terminal-file filepath)
  (filter string-full? (string-split (read-file filepath) #\newline)))

(define (execute-terminal-file filepath)
  (define (exe output)
    (let ((type      (term-type output))
          (formatted (term-format output)))
      (cond ((and (eq? type 'cmd) (not (eq? (car formatted) 'ls)))
             (system-files (car formatted) (cadr formatted)))
            ((eq? type 'dir) (system-files 'mkdir (second formatted)))
            ((eq? type 'file) (system-files 'touch (second formatted) (first formatted))))))
  (map exe (read-terminal-file filepath)))
