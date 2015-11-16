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

;; Test boxes
(compute-line sampletable 0)
;(compute-line sampletable 1)
;(compute-line sampletable 2)
(compute-line sampletable 3)
;(compute-line sampletable 4)
;(compute-line sampletable 5)
(compute-line sampletable 6)
;(compute-line sampletable 7)
;(compute-line sampletable 8)

;(compute-box (compute-line sampletable 0) 0)
;(compute-box (compute-line sampletable 0) 0)
;(compute-box (compute-line sampletable 0) 0)
;(compute-box (compute-line sampletable 0) 0)

(compute-boxes sampletable 0)
;; Test nth-column, first column
;(compute-columns sampletable 9)

;; Test transformation
;(transformTable sampletable)

