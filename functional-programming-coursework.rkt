#lang racket
(require racket/trace)

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

(define sampletable2 `(
                 [0 0 0 2 6 0 7 0 1]
                 [6 8 0 0 7 0 0 9 0]
                 [1 9 0 0 0 4 5 0 0]
                 [8 2 0 1 0 0 0 4 0]
                 [0 0 4 6 0 2 9 0 0]
                 [0 5 0 0 0 3 0 2 8]
                 [0 0 9 3 0 0 0 7 4]
                 [0 4 0 0 5 0 0 3 6]
                 [7 0 3 0 1 8 0 0 0]))

(define sampletable3 `(
                 [0 1 0 9 0 7 0 0 5]
                 [4 0 9 0 0 0 2 0 7]
                 [8 7 6 4 0 0 0 0 0]
                 [0 2 7 0 9 0 0 3 4]
                 [0 0 0 6 0 3 0 0 0]
                 [6 9 0 0 2 0 8 5 0]
                 [0 0 0 0 0 1 7 4 8]
                 [2 0 4 0 0 0 5 0 9]
                 [7 0 0 5 0 9 0 2 0]))

;; =================================================================================
;; UTILITY FUNCTION WORKING WITH COORDINATES
;; =================================================================================

;; Returns the table resulting from the application of the function
;; (func line column cell) to the cell at coordinate line, column
(define (func-on-coordinate func table line column)
  
  (define (pvt-line table (line-idx 1))

    (define (pvt-column line (column-idx 1))
      (if [null? line]
          null
          (cons
           (func line-idx column-idx (car line))
           (pvt-column (cdr line) (increment column-idx)))))

    ;; Entry point pvt-line
    (if (null? table)
        null
        (cons
         (pvt-column (car table))
         (pvt-line (cdr table) (increment line-idx)))))

  ; Entry point func-on-coordinate
  (pvt-line table))
  
;; The function (func line column cell) is called on each cell.
;; The function stops when (func) evaluates to a value different than false and
;; such value is returned, otherwise the end of the table is reached and the function
;; returns false, since the (func) predicate it is false for every cell.
(define (or-on-coordinate func table)
  
  (define (pvt-line table (line-idx 1))

    (define (pvt-column line (column-idx 1))
      ; Termination condition
      (if (null? line) #f
          ; Check func's return value, stop if true go ahead otherwise
          (let ([result (func  line-idx column-idx (car line))])
            (if result
                result
                (pvt-column (cdr line) (increment column-idx))))))
    ; pvt-line entry point
    (if (null? table) #f
        (let ([result (pvt-column (car table))])
          (if (not result)
              (pvt-line (cdr table) (increment line-idx))
              result))))
  ; or-on-coordinate entry line
  (pvt-line table))

;; The function (func line column cell) is called on each cell at a given line.
;; The function stops when (func) evaluates to a value different than false and
;; such value is returned, otherwise the end of the line is reached and the function
;; returns false, since the (func) predicate it is false for every cell in the line.
(define (or-on-line func table line-index)
  
  (define (pvt-line line (column-index 1))
    ;; Termination condition, end of line
    (if (null? line)
        #f
        ; Check func's return value, stop if true go ahead otherwise
        (let ([result (func column-index (car line))])
          (if result
              result
              (pvt-line (cdr line) (increment column-index))))))
  (pvt-line (list-ref table (- line-index 1))))

;; The function (func line column cell) is called on each cell at a given column.
;; The function stops when (func) evaluates to a value different than false and
;; such value is returned, otherwise the end of the column is reached and function
;; returns false, since the (func) predicate it is false for every cell in the line.
(define (or-on-column func table column-index)
  (define (pvt-line line (line-index 1))

    ;; End of table
    (if (null? line)
        #f
        (let ([result (func line-index (list-ref (car line) (- column-index 1)))])
          (if result
              result
              (pvt-line (cdr line) (increment line-index))))))
  (pvt-line table))


;; Transform a table of singletons representing a sudoku puzzle in a table where
;; for every input table's cell whose value was zero there is now a set composed
;; by all numbers from one to nine.
(define (transformTable table)
  ; Transform a cell, two cases:
  ; 1 - cell is a singelton, identity function
  ; 2 - cess is zero, return set (1 2 3 4 5 6 7 8 9)
  (define (transformNumber x)
    (if (> x 0)
        x
        (apply list `(1 2 3 4 5 6 7 8 9))))

  ;; Transform an entire row
  (define (transformRaw x)
    (if (empty? x)
        null
        (cons (transformNumber (car x)) (transformRaw (cdr x)))))
  
  ;; transformTable entry point
  (if (empty? table)
      null
      (cons (transformRaw (car table)) (transformTable (cdr table)))))

(define (atom? x)
  (not (or (pair? x) (null? x))))

;; Return the box's index for a given pair of cell's coordinate, the return
;; value start index is one.
(define (box-index line column)
  (let ((line-1 (- line 1))
        (column-1 (- column 1)))
    (+ 
     (* (truncate (/ line-1 3)) 3)
     (truncate (/ column-1 3))
     1)))
    
;; Merely increment a number
(define (increment number)
  (+ number 1))

;; The singleton list is composed by triples in the form:
;; (line, column, number).
;; This function builds a new triple with the given argument and add them at the
;; beginning of the list of triples passed as fourth argument.
(define (add-singleton line column number visited-singleton)
    (cons (list line column number) visited-singleton))


;; =================================================================================
;; FIND SINGLETON
;; =================================================================================

;; Searches for the next singleton in the table passed as argument which is not
;; already in the visited-singleton list
(define (find-singleton table visited-singleton)
  (or-on-coordinate (lambda (line column cell)
                      ;; Not a singleton
                      (if (list? cell) #f
                          (let ([singleton-item (list line column cell)])
                            ;; Singleton already present in the list
                            (if (member singleton-item visited-singleton)
                                #f
                                ;; Return the found item
                                (cons singleton-item visited-singleton)))))
                    table))

;; Removes the singleton number from the set cell, set containing a single
;; element as a result of this operation are reduced to a singleton.
(define (remove-singleton-atom cell number)
  (let ((reduced-list (filter (lambda (x) (not (= number x))) cell)))
    (if (= (length reduced-list) 1)
        (car reduced-list)
        reduced-list)))
                         
;; Handles a cell, which can be a singleton or a list
(define (remove-singleton-element entry number)
  (if (atom? entry)
      entry
      (remove-singleton-atom entry number)))

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
        (if (= acc lineidx)
            (cons (remove-singleton-line (car line)) (remove-singleton-table-line-pvt (cdr line) (+ acc 1)))
            (cons (car line) (remove-singleton-table-line-pvt (cdr line) (increment acc))))))
  (remove-singleton-table-line-pvt table 1))
;; =================================================================================
;; LINES SINGLETON END 
;; =================================================================================
;; =================================================================================
;; COLUMN SINGLETON REMOVAL 
;; =================================================================================
(define (remove-singleton-table-column table column number)
  (if (empty? table)
      null
      (cons (remove-singleton-column (car table) column number) (remove-singleton-table-column (cdr table) column number))))
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
;; FIRST STEP
;; =================================================================================
(define (get-line singleton-list)
  (car (car singleton-list)))

(define (get-column singleton-list)
  (car (cdr (car singleton-list))))

(define (get-number singleton-list)
  (car (cdr (cdr (car singleton-list)))))

;; Remove the singleton given as argument from every line, column or box where
;; it resides in.
(define (reduce table singleton line column)
  (define box-number (box-index line column))
  (remove-singleton-table-line
    (remove-singleton-table-column
     (remove-singleton-table-box table box-number singleton)
     column singleton)
    line singleton))

; Return the table and the list and the singleton's list, not in use
(define (first-step-single table singleton-list)
  (let ([current-step-singleton-list (find-singleton table singleton-list)])
    (if (not current-step-singleton-list)
        (values table #f)
        (values
         (reduce table
                 (get-number current-step-singleton-list)
                 (get-line current-step-singleton-list)
                 (get-column current-step-singleton-list))
         current-step-singleton-list))))

;; Perform the algorithm's first step, step by step
;; 1) Find a singleton which hasn't been discovered yet
;; 2) Remove it from every line,column,box it resides in
;; 3) Start again or stop if not more new singleton are found
(define (first-step table singleton-list)
  (if (not singleton-list)
      table
      ;; Recursive step with a reduced table
      (let((reduced-table (reduce
                           table
                           (get-number singleton-list)
                           (get-line singleton-list)
                           (get-column singleton-list))))
        (first-step reduced-table (find-singleton reduced-table singleton-list)))))

;; =================================================================================
;; END FIRST STEP
;; =================================================================================

;; =================================================================================
;; SECOND STEP
;; =================================================================================

;; Return the first singleton among the first table's cell composed by a set
;; which is not contained in the hashtable addressed by linecolumn values.
;; Hashtable's example : Key      Value
;;                       1      (12 13 14)
;;                       2      (21 22 23)
;; That hashtable describes a search on a table where the number
;; one has been returned, or discovered, at the values' coordinates expressed
;; as a pair of (line,column).
;;
;; The function uses such hashtable not to return twice the same value.

(define (find-singleton-set table visited-singleton-hashtable)
  (or-on-coordinate (lambda (line column cell)
                      
                      ;; I'm sure there is a better way to do it, but I am
                      ;; just a beginner so far
                      (define (add-it-and-return-it hashtable key value item)
                        ;; First element of the key's list
                        (if (null? value)
                            (hash-set! hashtable key (list item))
                            (hash-set! hashtable key (append value (list item))))
                        (list line column item))
                      ;; The cell contains a singleton, skip it
                      (if (atom? cell)
                          #f
                          (ormap (lambda (x)
                                   ;; Hashtable's key is the concatenation of line and column
                                   (let* ((key (string-append (number->string line) (number->string column)))
                                          (value (hash-ref visited-singleton-hashtable key #f)))
                                     ;; Key it is not present in the hashtable therefore it needs
                                     ;; to be returned and added to the hashtable
                                     (if (not value)
                                         (add-it-and-return-it visited-singleton-hashtable key null x)
                                         ;; If the item is present in the list of already visited
                                         ;; item for such key return false, return the item
                                         ;; itself otherwise
                                         (if (member x value)
                                             #f
                                             (add-it-and-return-it visited-singleton-hashtable key value x)))))
                                 cell)))
                    table))

(define (reduce-set line-set column-set singleton table)
  (func-on-coordinate (lambda (line column cell)
                        (if (and (= line line-set) (= column column-set))
                            singleton
                            cell))
                      table
                      line-set
                      column-set))
    
;; Checks if there is a set other than the one specified by the coordinates (line-idx, column-idx)
;; containing a specific number, this function it is used to check if it is possible to reduce
;; a set to a singleton.
(define (is-present-other-sets line-idx column-idx number table)
  (let* ([start-line (+ (truncate (/ (- line-idx 1) 3)) 1)]
         [start-column (+ (* 3 (truncate (/ (- column-idx 1) 3))) 1)]
         [end-line (+ start-line 2)]
         [end-column (+ start-column 2)])
    
    ;; Body
    (if 
     (or

     ;; Check for another set in this line containing the same number
     (not (or-on-line  (lambda (index cell)
                    ;; Return false if
                    ;; 1) the item is an atom
                    ;; 2) the column matches the index of the one being under check
                    ;; 3) set does not contain the number thus if count is zero
                    (if [or (atom? cell)
                            (= index column-idx)
                            (= (count (lambda (item) (= item number)) cell) 0)]
                        #f
                        #t))
                  table
                  line-idx))
     
     ;; Check for another set in this column containing the same number
     (not (or-on-column (lambda (index cell)
                     (if [or
                          (atom? cell)
                          (= index line-idx)
                          (= (count (lambda (item) (= item number)) cell) 0)]
                         #f
                         #t))
                   table
                   column-idx))
                     
     ;; Passing at the function the coordinated of the top-left corner of the box
     (not (or-on-coordinate (lambda (lindex cindex cell)
                         (cond
                           ; Not a set
                           ([atom? cell] #f)
                           ; Outside box's boundaries
                           ([or
                             ; Outside box's line boundaries
                             (or (< lindex start-line) (> lindex end-line))
                             ; Outsude box's column boundaries
                             (or (< cindex start-column) (> cindex end-column))]
                            #f)
                           ; We are looking for a set other than this cell, skip it
                           ([and (= lindex line-idx) (= cindex column-idx)] #f)
                           ; It does not contain the target number
                           ([not (member number cell)] #f)
                           (else
                            #t)))
                            table)))
     #f
     #t)))
          

; Return the table and the list and the set's singleton list
(define (second-step table visited-singleton-set)
  (let ([current-singleton (find-singleton-set table visited-singleton-set)])
    (cond
      ([not current-singleton] table)
      ; This singleton is not unique, search for a new one
      ([is-present-other-sets (list-ref current-singleton 0)
                              (list-ref current-singleton 1)
                              (list-ref current-singleton 2)
                              table]
       (second-step table visited-singleton-set))
      (else
       (reduce-set (list-ref current-singleton 0) (list-ref current-singleton 1) (list-ref current-singleton 2) table)))))


;; =================================================================================
;; SECOND STEP END
;; =================================================================================

;; =================================================================================
;; RESOLVER
;; =================================================================================

; Return true only if every cell is a singleton
(define (solver-termination-condition table)
  (let (
        (result (or-on-coordinate (lambda (line column cell)
                                    (if (not (atom? cell)) #t
                                        #f))
                                  table)))
    (not result)))

(define (solve matrix)
  (define (pvt-solve transformed-matrix)
    (if (solver-termination-condition transformed-matrix)
        transformed-matrix
        (let* ([first-step-table (first-step transformed-matrix (find-singleton transformed-matrix null))]
               [second-step-table (second-step first-step-table (make-hash))])
          (pvt-solve second-step-table)
          )))
  (pvt-solve (transformTable matrix)))
        
    
    



; Tests
;(define lines (transformTable sampletable))
;(first-step-single lines null)
;(first-step lines (find-singleton lines null))
;(second-step lines (make-hash))
;(solve sampletable3)
;(solve sampletable2)
;(solve sampletable)


;; Exports
(provide transformTable
         atom?
         box-index
         find-singleton
         add-singleton
         remove-singleton-element
         remove-singleton-column
         remove-singleton-table-column
         remove-singleton-table-line
         remove-singleton-table-box
         reduce
         get-line
         get-column
         get-number
         increment
         is-present-other-sets
         func-on-coordinate
         or-on-coordinate
         or-on-line
         or-on-column
         find-singleton-set
         reduce-set
         second-step
         solver-termination-condition
         first-step
         )


