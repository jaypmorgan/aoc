;; --- Day 3: Rucksack Reorganization ---

;; One Elf has the important job of loading all of the rucksacks with
;; supplies for the jungle journey. Unfortunately, that Elf didn't
;; quite follow the packing instructions, and so a few items now need
;; to be rearranged.

;; Each rucksack has two large compartments. All items of a given type
;; are meant to go into exactly one of the two compartments. The Elf
;; that did the packing failed to follow this rule for exactly one
;; item type per rucksack.

;; The Elves have made a list of all of the items currently in each
;; rucksack (your puzzle input), but they need your help finding the
;; errors. Every item type is identified by a single lowercase or
;; uppercase letter (that is, a and A refer to different types of
;; items).

;; The list of items for each rucksack is given as characters all on a
;; single line. A given rucksack always has the same number of items
;; in each of its two compartments, so the first half of the
;; characters represent items in the first compartment, while the
;; second half of the characters represent items in the second
;; compartment.

;; For example, suppose you have the following list of contents from
;; six rucksacks:

;; vJrwpWtwJgWrhcsFMMfFFhFp
;; jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
;; PmmdzqPrVvPwwTWBwg
;; wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
;; ttgJtRGJQctTZtZT
;; CrZsJsPPZsGzwwsLwLmpwMDw

;; - The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp,
;; which means its first compartment contains the items vJrwpWtwJgWr,
;; while the second compartment contains the items hcsFMMfFFhFp. The
;; only item type that appears in both compartments is lowercase p.
;; - The second rucksack's compartments contain jqHRNqRjqzjGDLGL and
;; rsFMfFZSrLrFZsSL. The only item type that appears in both
;; compartments is uppercase L.
;; - The third rucksack's compartments contain PmmdzqPrV and
;; vPwwTWBwg; the only common item type is uppercase P.
;; - The fourth rucksack's compartments only share item type v.
;; - The fifth rucksack's compartments only share item type t.
;; - The sixth rucksack's compartments only share item type s.

;; To help prioritize item rearrangement, every item type can be
;; converted to a priority:

;; - Lowercase item types a through z have priorities 1 through 26.
;; - Uppercase item types A through Z have priorities 27 through 52.

;; In the above example, the priority of the item type that appears in
;; both compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22
;; (v), 20 (t), and 19 (s); the sum of these is 157.

;; Find the item type that appears in both compartments of each
;; rucksack. What is the sum of the priorities of those item types?

;;;------------------------------ Solution -------------------------------

(load "lib.scm")

(define test-file "data/day3.test.txt")

;; File reading and formatting functions, we want to take a list of
;; sacks, where each sack is on it's own line, and be able to read
;; the left and right compartments of each sack.

;; get the first compartment for a given sack (i.e. the left part
;; of the sack)
(define (first-compartment sack)
  (take (/ (length sack) 2) sack))

;; get the second compartment for a given sack (i.e. the right
;; half of the sack string)
(define (second-compartment sack)
  (reverse (take (/ (length sack) 2) (reverse sack))))

;; given a sack, return a list of the two compartments split.
(define (split-compartments sack)
  (list (first-compartment sack)
	(second-compartment sack)))

;; read and format the sack list from a file
(define (read-rucksack-file filepath)
  (map split-compartments
       (map string->list  ;; convert the sack into a list of items
	    (filter (lambda (sack) (not (string= sack ""))) ;; remove empty sack lines
		    (string-split (read-file filepath) #\newline)))))

;; Functions to find the common elements per compartments. We first
;; define a function that will find the unique elements of a
;; list. This is to make the next function, union, slightly more
;; efficient by checking membership of a type of element once.
;; Finally, we create two functions, find-common-element and its list
;; counterpart that finds the element in both compartments of the
;; sacks.

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

;; find the common element between two compartments of a sack
(define (find-common-element sack)
   (first
    (intersection (unique (first sack))
		  (unique (second sack)))))

;; find the common elements in many sacks
(define (find-common-elements sacks)
  (map find-common-element sacks))

;; Functions to calculate the priorities

(define (inverse-case c)
  ;; inverse a character from lower to upper case or from upper case
  ;; to lower case.
  (if (char-upper-case? c)
      (char-downcase c)
      (char-upcase c)))

;; Find the priority number of a item i.e. #\p => 16
(define (priority el)
  (let ((offset (or (and (char-upper-case? el) 6) 0)))
    (- (- (char->integer (inverse-case el)) 64) offset)))

(define (sum-of-priorities sacks)
  (sum (map priority sacks)))

;; Solution to part one can be completed via
(sum-of-priorities
 (find-common-elements
  (read-rucksack-file "data/day3.input.txt")))

;;--- Part Two ---

;; As you finish identifying the misplaced items, the Elves come to
;; you with another issue.

;; For safety, the Elves are divided into groups of three. Every Elf
;; carries a badge that identifies their group. For efficiency, within
;; each group of three Elves, the badge is the only item type carried
;; by all three Elves. That is, if a group's badge is item type B,
;; then all three Elves will have item type B somewhere in their
;; rucksack, and at most two of the Elves will be carrying any other
;; item type.

;; The problem is that someone forgot to put this year's updated
;; authenticity sticker on the badges. All of the badges need to be
;; pulled out of the rucksacks so the new authenticity stickers can be
;; attached.

;; Additionally, nobody wrote down which item type corresponds to each
;; group's badges. The only way to tell which item type is the right
;; one is by finding the one item type that is common between all
;; three Elves in each group.

;; Every set of three lines in your list corresponds to a single
;; group, but each group can have a different badge item type. So, in
;; the above example, the first group's rucksacks are the first three
;; lines:

;; vJrwpWtwJgWrhcsFMMfFFhFp
;; jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
;; PmmdzqPrVvPwwTWBwg

;; And the second group's rucksacks are the next three lines:

;; wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
;; ttgJtRGJQctTZtZT
;; CrZsJsPPZsGzwwsLwLmpwMDw

;; In the first group, the only item type that appears in all three
;; rucksacks is lowercase r; this must be their badges. In the second
;; group, their badge item type must be Z.

;; Priorities for these items must still be found to organize the
;; sticker attachment efforts: here, they are 18 (r) for the first
;; group and 52 (Z) for the second group. The sum of these is 70.

;; Find the item type that corresponds to the badges of each three-Elf
;; group. What is the sum of the priorities of those item types?

(define (merge-compartments sack)
  (append (first sack) (second sack)))

(define (find-common-element sacks)
  (let ([sacks (map merge-compartments sacks)])
    (first
     (intersection (intersection (unique (third sacks)) (unique (second sacks)))
		   (unique (first sacks))))))

(define (find-common-elements sacks)
  (let loop ([sacks sacks] [cels (list)])
    (cond ((null? sacks) cels)
	  (else (loop (slice 3 (length sacks) 1 sacks)
		      (cons (find-common-element (take 3 sacks)) cels))))))

(sum-of-priorities
 (find-common-elements
  (read-rucksack-file "data/day3.input.txt")))
