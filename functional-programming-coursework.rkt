#lang racket
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
;; Transform an entire table
(define (transformTable table)
  ;; Transform a single number
  (define (transformNumber x)
    (if (> x 0)
        x
        (apply list `(1 2 3 4 5 6 7 8 9))))
  ;; Transform an entire row
  (define (transformRaw x)
    (if (empty? x)
        null
        (cons (transformNumber (car x)) (transformRaw (cdr x)))))
  ;; Use above functions to expand the whole table
  (if (empty? table)
      null
      (cons (transformRaw (car table)) (transformTable (cdr table)))))

;; Extract the n-th element (combine drop and take ?)
(define (extract list idx)
  (car (drop (take list idx) (- idx 1))))
  
;; Transform a list of lines into a list of columns
(define (compute-columns list length)
  ;; Compute the n-th column of the whole table
  (define (nth-column list idx)
    (if (empty? list)
        null
        (cons (extract (car list) idx) (nth-column (cdr list) idx))))
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

;; Utility function
(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (box-index line column)
  (+
   (* (truncate (/ line 3)) 3)
   (truncate (/ column 3))))

;; Add the singleton to the global list
(define (add-singleton line column number visited-singleton)
    (cons visited-singleton (list line column number)))

;; Check if a given singleton is already present in the global list
(define (is-singleton-present line column number visited-singleton)
  (define (singleton-equals? item)
    (and (= line (car item))
         (= column (car (cdr item)))
         (= number (car (cdr (cdr item))))))
  
  ; Function's entry point
  (ormap singleton-equals? visited-singleton))

;; Find a singleton given the whole
;; list, that is, a list of list
;; which could be composed by atom and
;; list ... TODO: improve comments
(define (find-singleton entry visited-singleton)
  (define singleton-number 0)
  (define line-number 0)
  (define column-number 0)

  (define (incr-line)
    (set! line-number (+ line-number 1)))

  (define (incr-column)
    (set! column-number (+ column-number 1)))
  
  (define (set-number number)
    (set! singleton-number  number))
    
  ;; Get the singleton and stores the column
  ;; entry is the first element of line list
  (define (contains-singleton entry)
    (define (is-singleton item)
      (if (and (atom? item) (> item 0) (not (is-singleton-present line-number column-number item visited-singleton)))
          (cons (list line-number column-number item) visited-singleton)
          (and (incr-column) #f)))

    ;; Function's entry point: iterate through columns
    (ormap is-singleton entry))

  ;; Entry is a line
  (define (find-singleton-line entry)
    (or (contains-singleton entry)
        (and (incr-line) (set! column-number 0) #f)))

  ;; Function's entry point
  (ormap find-singleton-line entry))

   
;; Removes the singleton from a the index fof the given entry table, whatever it is its representation (row, columns, boxes)
(define (remove-singleton entry number index)
  (define acc 0)
  ;; Removes the singleton from a single list composed ONLY by atoms
  (define (remove-singleton-atom list)
    (if (empty? list)
        null
        (if (= (car list) number)
            (remove-singleton-atom (cdr list))
            (cons (car list) (remove-singleton-atom (cdr list))))))
  ;; Remove the singleton from a list which represents a row, column, box
  ;; Please note that singleton elements are not removed from the "first level" list
  (define (remove-singleton-line list)
    (cond ((empty? list) null)
          ((atom? (car list)) (cons (car list) (remove-singleton-line (cdr list))))
          (else (cons (remove-singleton-atom (car list)) (remove-singleton-line (cdr list))))))
  ;; Skip all the index [0..index -1] and remove the singleton for the element index
  (define (remove-singleton-pvt entry acc)
    (cond ((empty? entry) null)
          ((= acc index) (cons (remove-singleton-line (car entry)) (remove-singleton-pvt (cdr entry) (+ acc 1))))
          (else (cons (car entry) (remove-singleton-pvt (cdr entry) (+ acc 1))))))

  ;; Real call TODO: check that index < length(entry)
  (remove-singleton-pvt entry acc))


(define (first-step lines columns boxes singletons)
  (if (not singletons)
      (print lines)
      (let (
            (lineidx (car (take (car singletons) 1)))
            (columnidx (car (take (drop (car singletons) 1) 1)))
            (number (car (drop (car singletons) 2))))
            ((first-step (remove-singleton lines number lineidx)
                   (remove-singleton columns number  columnidx)
                   (remove-singleton boxes number (box-index lineidx columnidx))
                   (find-singleton (remove-singleton lines number lineidx) singletons)
                   )))))

;; It's much more convenient to use a SINGLE table and scan it using different "prespective" as boxes, lines, columns

;; Exports
(provide transformTable extract compute-columns compute-boxes atom? box-index find-singleton remove-singleton add-singleton is-singleton-present remove-singleton)


;; Kind of a real beginning
;; List of nine lines, tranformed

;; Global data
(define lines (transformTable sampletable))
;;; List of nine columns, tranformed
(define columns (transformTable (compute-columns sampletable 9)))
;;; List of nine boxes, tranformed
(define boxes (transformTable (compute-boxes sampletable 0)))
(first-step lines columns boxes (find-singleton lines null))












;
;(find-singleton lines)
;(writeln line-number)
;(writeln column-number)
;(writeln (box-index (- line-number 1) (- column-number 1)))
;(writeln singleton-number)
;(remove-singleton lines singleton-number (- line-number 1))

;(find-singleton lines)
;(writeln line-number)
;(writeln column-number)
;(writeln singleton-number)
;(remove-singleton lines singleton-number (- line-number 1))
;(first-step lines columns boxes)
;(remove-singleton `(
;                    ; First
;                    (
;                     (1 2 3 4 5 6 7 8 9)
;                     (1 2 3 4 5 6 7 8 9)
;                     (1 2 3 4 5 6 7 8 9)
;                     (1 2 3 4 5 6 7 8 9)
;                     (1 2 3 4 5 6 7 8 9)
;                    )
;                    ; Second
;                    (
;                     8
;                     9
;                     1
;                     (1 2 3 4 5 6 7 8 9)
;                    )
;                    )
;                  ; Number to remove
;                  9
;                  ; Index
;                  2)
;
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
;(find-singleton TEST)
;(writeln line-number)
;(writeln column-number)
;(writeln (box-index line-number column-number))
;(writeln singleton-number)

;(writeln line-number)
;(writeln column-number)
;(writeln singleton-number)
;(remove-singleton TEST singleton-number)
;(box-index (- line-number 1) (- column-number 1))
;(find-singleton (transformTable (compute-columns sampletable 9)))
;(find-singleton (transformTable (compute-boxes sampletable 0)))

;(let ((x 3) (y (+ x 1))) (+ x y))
