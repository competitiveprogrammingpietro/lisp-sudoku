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

;; Extract the n-th element from a list
(define (extract list idx)
  (define (extract-pvt entry (acc 1))
    (cond
      ([empty? entry] null)
      ([= idx acc] [car entry])
      (else (extract-pvt (cdr entry) (+ acc 1)))))
  (extract-pvt list))
                       
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

;; List of lines into lists of boxes of a fixed size
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
      ;; Why do I need that ?
      (apply append
             (list 
              (take (compute-line-offset (car entry) index) 3)
              (take (compute-line-offset (car (cdr entry)) index) 3)
              (take (compute-line-offset (car (cdr (cdr entry))) index) 3))))
    (compute-box-pvt entry index))
  
  (if (> acc 8)
      null
      (cons (compute-box (compute-line entry acc) acc) (compute-boxes entry (+ acc 1)))))


;; Utility functions
(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (box-index line column)
  (+
   (* (truncate (/ line 3)) 3)
   (truncate (/ column 3))
   1))

;; Common function for increment
(define (increment number)
  (+ number 1))

;; Add the singleton to the global list
(define (add-singleton line column number visited-singleton)
    (cons (list line column number) visited-singleton))

;; Check if a given singleton is already present in the global list
(define (is-singleton-present line column number visited-singleton)
  (define (singleton-equals? item)
    (and (= line (car item))
         (= column (car (cdr item)))
         (= number (car (cdr (cdr item))))))
  
  ; Function's entry point
  (ormap singleton-equals? visited-singleton))


;; =================================================================================
;; FIND SINGLETON
;; Return a list (line-number column-number item)
;; =================================================================================
(define (find-singleton entry visited-singleton)
  (define singleton-number 0)
  (define line-number 1)
  (define column-number 1)

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
        (and (incr-line) (set! column-number 1) #f)))

  ;; Function's entry point
  (ormap find-singleton-line entry))

;; =================================================================================
;; FIND SINGLETON END
;; =================================================================================


;; Handles a simple list, no pairs are admitted in here (GROUP)
(define (remove-singleton-atom list number)
  (if (empty? list)
      null  
      (if (= (car list) number)
          (remove-singleton-atom (cdr list) number)
          (cons (car list) (remove-singleton-atom (cdr list) number)))))

;; Remove a singleton from a list's element, it acts as switch
;; handling the case when the element is composed by a single element
;; as well when it is composed by a list
(define (remove-singleton-element entry number)
  (if (atom? entry)
      entry
      (remove-singleton-atom entry number)))

;; =================================================================================
;; REMOVE SINGLETON FROM A LIST
;; =================================================================================
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
;; =================================================================================
;; END REMOVE SINGLETON FROM A LIST
;; =================================================================================

;; =================================================================================
;; COLUMN SINGLETON REMOVAL 
;; =================================================================================
(define (remove-singleton-table-column table column number)
  (if (empty? table)
      null
      (cons (remove-singleton-column (car table) column number) (remove-singleton-table-column (cdr table) column number))))
;; Remove a sigleton from a column given a line
(define (remove-singleton-column entry column number)
  
  (define (remove-singleton-column-pvt entry acc)
    (if (empty? entry)
        null
        (if (= acc column)
            (cons (remove-singleton-element (car entry) number) (remove-singleton-column-pvt (cdr entry) (+ acc 1)))
            (cons (car entry) (remove-singleton-column-pvt (cdr entry) (+ acc 1))))))
        
  ;; Entry point
  (remove-singleton-column-pvt entry 1))
;; =================================================================================
;; END COLUMN 
;; =================================================================================


;; =================================================================================
;; LINES SINGLETON REMOVAL 
;; =================================================================================
(define (remove-singleton-table-line table lineidx number)

  (define (remove-singleton-line line)
    (if (empty? line)
        null
        (cons (remove-singleton-element (car line) number) (remove-singleton-line (cdr line)))))

  (define (remove-singleton-table-line-pvt line acc)
    (if (empty? line)
        null
        (if (= acc (- lineidx 1))
            (cons (remove-singleton-line (car line)) (remove-singleton-table-line-pvt (cdr line) (+ acc 1)))
            (cons (car line) (remove-singleton-table-line-pvt (cdr line) (+ acc 1)))
            )))

  (remove-singleton-table-line-pvt table 0))
;; =================================================================================
;; LINES SINGLETON END 
;; =================================================================================


;; =================================================================================
;; BOXES SINGLETON REMOVAL 
;; =================================================================================
(define (remove-singleton-table-box table box number)
  (define line-number (* (truncate (/ (- box 1) 3)) 3))
  (define line-offset (* (modulo (- box 1) 3) 3))
   
  (define (remove-singleton-table-box-line-pvt line acc)
    (if (empty? line)
        null
        (if (or (= acc line-number) (= acc (+ line-number 1)) (= acc (+ line-number 2)))
            (cons (remove-singleton-table-box-line-offset (car line) 0)
                  (remove-singleton-table-box-line-pvt (cdr line) (+ acc 1)))
            (cons (car line )(remove-singleton-table-box-line-pvt (cdr line) (+ acc 1))))))
           
  ;; Process a sigle line
  (define (remove-singleton-table-box-line-offset line acc)
    (if (empty? line)
        null 
        ;; Process the three elements
        (if (or (= acc line-offset) (= acc (+ line-offset 1)) (= acc (+ line-offset 2)))
            (cons (remove-singleton-element (car line) number)
                  (remove-singleton-table-box-line-offset (cdr line) (+ acc 1)))
            (cons (car line) (remove-singleton-table-box-line-offset (cdr line) (+ acc 1))))))


  ;; Entry Point
  (remove-singleton-table-box-line-pvt table 0))
;; =================================================================================
;; BOXES SINGLETON REMOVAL END 
;; =================================================================================


;; =================================================================================
;; IS PRESENT FAMILY FUNCTIONS
;; =================================================================================
(define (is-present-item item number)
  (if (atom? item)
      #f
      (member number item)))

;; Search for line
(define (is-present-line table number lineidx columnidx)
  (define (is-present-line-pvt line accC)
    (cond ((empty? line) #f)
          ((= accC columnidx) (is-present-line-pvt (cdr line) (+ accC 1)))
          ((is-present-item (car line) number) #t)
          (else (is-present-line-pvt (cdr line) (+ accC 1)))))
  (is-present-line-pvt (car (drop (take table lineidx) (- lineidx 1))) 1))

;; Search column
(define (is-present-column table number lineidx columnidx)
  (define (is-present-column-line-pvt line)
    (is-present-item (car (drop line (- columnidx 1))) number))
  (define (pvt line acc)
    (cond
      ;; Termination case
      [(empty? line) #f]
      ;; Skip cell
      [(= acc lineidx) (pvt (cdr line) (+ acc 1))]
      ;; Check cell
      [(is-present-column-line-pvt (car line)) #t]
      ;; Keep searching
      [else (pvt (cdr line) (+ acc 1))]))
  (pvt table 1))

;; Search boxes, not the most efficient due to the copy
(define (is-present-box table number lineidx columnidx)
  (define boxidx (box-index lineidx columnidx))
  (define boxes-table (compute-boxes table 0))
  
  ;; Compute where the skip cell is 
  (define cellidx (+ (* (- (modulo lineidx 3) 1) 3) (modulo columnidx 3)))

  (define (is-present-box-pvt line acc)
    (cond
      ;; Termination case
      [(empty? line) #f]
      ;; Skip cell
      [(= acc cellidx) (is-present-box-pvt (cdr line) (+ acc 1))]
      ;; Check cell
      [(is-present-item (car line) number) #t]
      ;; Continue searching with another cell
      [else (is-present-box-pvt (cdr line) (+ acc 1))]))
     
  (define (pvt line acc)
    (cond
      [(empty? line) #f]
      [(= acc boxidx) (is-present-box-pvt (car line) 1)]
      [else (pvt (cdr line))]))

  ;(pvt boxes-table 1))
  (is-present-box-pvt (drop (take boxes-table boxidx) (- boxidx 1)) 1))
    
;; =================================================================================
;; FIRST STEP
;; =================================================================================
(define (get-line singleton-list)
  (car (car singleton-list)))

(define (get-column singleton-list)
  (car (cdr (car singleton-list))))

(define (get-number singleton-list)
  (car (cdr (cdr (car singleton-list)))))

(define (reduce table singleton line column)
  (define box-number (box-index line column))
  (remove-singleton-table-line
    (remove-singleton-table-column
     (remove-singleton-table-box table box-number singleton)
     column singleton)
    line singleton))

(define (first-step table singleton-list)
  (if (not singleton-list)
      table
      ;; Recursive step with an reduced table
      (first-step
       (reduce
        table
        (get-number singleton-list)
        (get-line singleton-list)
        (get-column singleton-list))
       (find-singleton table singleton-list))))
;; =================================================================================
;; END FIRST STEP
;; =================================================================================

;; MAIN TEST
;(define lines (transformTable sampletable))
;(first-step lines (find-singleton lines null))



;; Exports
(provide
 transformTable
 extract
 compute-columns
 compute-boxes
 atom?
 box-index
 find-singleton
 remove-singleton
 add-singleton
 is-singleton-present
 remove-singleton-element
 remove-singleton-column
 remove-singleton-table-column
 remove-singleton-table-line
 remove-singleton-table-box
 reduce
 get-line
 get-column
 get-number
 is-present-line
 is-present-column
 is-present-box
 increment
 )


;; Kind of a real beginning
;; List of nine lines, tranformed

;; Global data
;(define lines (transformTable sampletable))
;(define singleton_list null)
  
;;; List of nine columns, tranformed
;(define columns (transformTable (compute-columns sampletable 9)))
;;; List of nine boxes, tranformed
;(define boxes (transformTable (compute-boxes sampletable 0)))
;(first-step lines columns boxes (find-singleton lines null))
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
