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
  
      

;; Test extrac()
;(extract `(0 2 5 0 0 1 0 0 0) 1 1)

;; Test nth-column, first column
;(nth-column sampletable 1)
;(reverse (compute-columns sampletable 9))
(compute-columns sampletable 9)


;; Test lines -> column
;(compute-column sampletable)


;(transformRaw `(0 2 5 0 0 1 0 0 0))
;(transformTable sampletable)

