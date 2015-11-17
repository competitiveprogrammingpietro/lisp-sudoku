#lang racket

;; Sample of a 9x9 Sudoku table
(define sampletable `(
                 [0 2 5 0 0 1 0 0 0]
                 [1 0 4 2 5 0 0 0 0]
                 [0 0 6 0 0 4 2 1 0]
                 [0 5 0 0 0 0 3 2 0]
                 [6 0 0 0 2 0 0 0 9]
                 [0 8 7 0 0 0 0 6 0]
                 [0 9 1 5 0 0 6 0 0]
                 [0 0 0 0 7 8 1 0 3]
                 [0 0 0 6 0 0 5 9 0]))

; This row represents all the possibilities, that is, all the numbers from 1 to 9
(define TBD `(1 2 3 4 5 6 7 8 9))

(define TEST `(
               ; 1
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 2
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 3
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 4
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 5
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) 8 (1 2 3 4 5 6 7 8 9))
               ; 6
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 7
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 8
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 9
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))              
               ))

;; Transform an entire table table
(define (transformTable table)

  ;; Transform a single number
  (define (transformNumber x)
    (if (> x 0)
        x
        (apply list TBD)))

  ;; Transform an entire row
  (define (transformRaw x)
    (if (empty? x)
        null
        (cons (transformNumber (car x)) (transformRaw (cdr x)))))
  
  ;; Use above functions to expand the whole table
  (if (empty? table)
      null
      (cons (transformRaw (car table)) (transformTable (cdr table)))))



;; Extract the n-th element
(define (extract list idx acc)
  (if (= acc idx)
      (car list)
      (extract (cdr list) idx (+ acc 1))))


;; Transform a list of lines into a list of columns
(define (compute-columns list length)

  ;; Compute the n-th column of the whole table
  (define (nth-column list idx)
    (if (empty? list)
        null
        (cons (extract (car list) idx 1) (nth-column (cdr list) idx))))

  ;; Compute a list of column from a list of lines
  ;; the accumulator specifies the length of the list
  (define (compute-columns-r list length)
    (if (= length 0)
        null
        (cons (nth-column list length) (compute-columns-r list (- length 1)))))

  ;; Return the reverse of it
  (reverse (compute-columns-r list length)))



(define (compute-boxes entry acc)

  (define (compute-offset index)
    (if (> index 8)
        -1
        (* (modulo index 3) 3)))

  (define (compute-raw index)
    (if (> index 8)
        -1
        (* (truncate (/ index 3)) 3)))

  ;; Return a pointer to the first line
  ;; which is included in the index-th box
  (define (compute-line entry index)
    (if (empty? entry)
        null
        (drop entry (compute-raw index))))

  ;; Return a pointer to the first element
  ;; of that line contained in the index-th
  ;; box
  ;; The entry parameter points to the line
  ;; number (index / 3) * 3
  (define (compute-line-offset entry index)
    (drop entry (compute-offset index)))

  ;; Create a single box given the pointer
  (define (compute-box entry index)
    (define (compute-box-pvt entry index)
      (list (take (compute-line-offset (car entry) index) 3)
            (take (compute-line-offset (car (cdr entry)) index) 3)
            (take (compute-line-offset (car (cdr (cdr entry))) index) 3)))
    (flatten (compute-box-pvt entry index)))

  (if (> acc 8)
      null
      (cons (compute-box (compute-line entry acc) acc) (compute-boxes entry (+ acc 1)))))

(define (atom? x)
  (not (or (pair? x) (null? x))))

;; Global data
(define singleton-number 0)
(define line-number 1)
(define column-number 1)

(define (box-index line column)
  (+
   (* (truncate (/ line 3)) 3)
   (truncate (/ column 3))))

;; Find a singleton given the whole
;; list, that is, a list of list
;; which could be composed by atom and
;; list ... TODO: improve comments
(define (find-singleton entry)
  (define (incr-line)
    (set! line-number (+ line-number 1)))
  (define (incr-column)
    (set! column-number (+ column-number 1)))
  (define (set-number number)
    (set! singleton-number  number))
  
  ;; Get the singleton and stores the column
  ;; Entry is a line list
  (define (contains-singleton entry)
    (define (is-singleton item)
      (if (and (atom? item) (> item 0))
          (set-number item)
          (and (incr-column) #f)))
    ;; Iterate through columns
    (ormap is-singleton entry))

  ;; Entry is the a line
  (define (find-singleton-line entry)
    (if (contains-singleton entry) #t
        (and (incr-line) (set! column-number 1) #f)))
    
  (ormap find-singleton-line entry))

;; Expects a list containing list of list of {list, atom} which is the representation
;; of a whole table, USELESS
(define (remove-singleton list number)
  
  ;; Removes the singleton from a single list composed ONLY by atoms
  ;; THIS IS WHAT I WANTED
  (define (remove-singleton-atom list)
    (if (empty? list)
        null
        (if (= (car list) number)
            (remove-singleton-atom (cdr list))
            (cons (car list) (remove-singleton-atom (cdr list)))))) 

  ;; Removes the singleton from a single list composed by atoms and pairs, that is, other lists
  (define (remove-singleton-pair entry)
    (if (empty? entry)
        null
        (if (atom? (car entry))
            (cons (car entry) (remove-singleton-pair (cdr entry))) 
            (cons (remove-singleton-atom (car entry)) (remove-singleton-pair (cdr entry)))))) 
  (if (empty? list)
      null
      (cons (remove-singleton-pair (car list)) (remove-singleton (cdr list) number))))

;(remove-singleton-two `(1 2 3 4 (5 6 7) 8 9 (5 6 7)) 5)
(remove-singleton `(
        (1 2 3 4 (5 6 7) 8 9 (5 6 7))
        (1 2 3 4 (5 6 7) 8 9 (5 6 7))
        (1 2 3 4 (5 6 7) 8 9 (5 6 7))
        (1 2 3 4 (5 6 7) 8 9 (5 6 7))
        )
      5)

;; A singleton has been found: if greater than zero return it otherwise
        ;; keep searching
        ;((atom? (car entry)) (if (> (car entry) 0)
         ;                        (set-number (car entry))
          ;                       (contains-singleton (cdr entry))))
        
  
;; Test boxes
;(compute-line sampletable 0)
;(compute-line sampletable 1)
;(compute-line sampletable 2)
;(compute-line sampletable 3)
;(compute-line sampletable 4)
;(compute-line sampletable 5)
;(compute-line sampletable 6)
;(compute-line sampletable 7)
;(compute-line sampletable 8)
;(compute-box (compute-line sampletable 0) 0)
;(compute-box (compute-line sampletable 0) 0)
;(compute-box (compute-line sampletable 0) 0)
;(compute-box (compute-line sampletable 0) 0)

;; Test transform
;(transformTable sampletable)

;; Test nth-column, first column
;(compute-columns sampletable 9)
;(transformTable (compute-columns sampletable 9))

;; Test compute boxes
;(compute-boxes sampletable 0)
;(transformTable (compute-boxes sampletable 0))


;; Test find-singleton
;(find-singleton (transformTable sampletable))
(find-singleton TEST)
(writeln line-number)
(writeln column-number)
(writeln singleton-number)
(remove-singleton TEST singleton-number)
;(box-index (- line-number 1) (- column-number 1))
;(find-singleton (transformTable (compute-columns sampletable 9)))
;(find-singleton (transformTable (compute-boxes sampletable 0)))

;(let ((x 3) (y (+ x 1))) (+ x y))