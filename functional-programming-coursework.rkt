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

;; Return the table resulting from the application of the function (func line column cell)
;; to each cell cell
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
  
;; For each table's cell the function (func line column cell)
;; is called on each cell.
;; The function stops when (func) evaluates to a value different
;; than false and  such value is returned, it reaches the end
;; of the table returning false otherwise.
return the (func line column table[line*stride + column])
(define (or-on-coordinate func table)
  
  (define (pvt-line table (line-idx 1))

    (define (pvt-column line (column-idx 1))
      ; Termination condition
      (if (null? line) #f
          ; Otherwise we check the func returned value
          (let ([result (func  line-idx column-idx (car line))])
            ; Function retured true, stop
            (if result
                result
                ; Else: recursive call
                (pvt-column (cdr line) (increment column-idx))))))

    (if (null? table) #f
        (let ([result (pvt-column (car table))])
          (if (not result)
              (pvt-line (cdr table) (increment line-idx))
              result))))
  (pvt-line table))
  
;; Executes func for each cell such that {table[line-index[0]] .. table[line-index[N]]}
;; where N {0 .. line-size}
;; (func index table[line-index[index]])
(define (or-on-line func table line-index)
  
  (define (pvt-line line (column-index 1))
    ;; End of the line
    (if (null? line)
        #f
        (let ([result (func column-index (car line))])
          (if result
              result
              (pvt-line (cdr line) (increment column-index))))))

  (pvt-line (list-ref table (- line-index 1))))

;; Executes func for each cell such that {table[0[column-index]] ... table[N[column-index]]}
;; where N {0 .. table-size}
;; (func index table[current-line[column-index]])

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

;; Transform a table
(define (transformTable table)
    ;; Transform a single number
  (define (transformNumber x)
    (if (> x 0)
        x
        `(1 2 3 4 5 6 7 8 9)))
  ;; Transform an entire row
  (define (transformRaw x)
    (if (empty? x)
        null
        (cons (transformNumber (car x)) (transformRaw (cdr x)))))
  ;; Use above functions to expand the whole table
  (if (empty? table)
      null
      (cons (transformRaw (car table)) (transformTable (cdr table)))))

;;TODO: There is the pair? operator!
(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (box-index line column)
  (let ((line-1 (- line 1))
        (column-1 (- column 1)))
    (+ 
     (* (truncate (/ line-1 3)) 3)
     (truncate (/ column-1 3))
     1)))
    
;; Common function for increment
(define (increment number)
  (+ number 1))

;; Add the singleton to the global list
(define (add-singleton line column number visited-singleton)
    (cons (list line column number) visited-singleton))



;; =================================================================================
;; FIND SINGLETON
;; =================================================================================
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

;; Handles a set cell, namely a single level list
;; Sets containing a single number are reduced to a singleton
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

(define (reduce table singleton line column)
  (define box-number (box-index line column))
  (remove-singleton-table-line
    (remove-singleton-table-column
     (remove-singleton-table-box table box-number singleton)
     column singleton)
    line singleton))

; Return the table and the list and the singleton's list
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

; Notes on find-singleton

; Ideally an hast-table mapping (coordinates) -> {already visited singleton}
; wouldn't be a bad idea
; Struct {recent} {Hashtable}

; Reduce singleton
; Check condition: each cell is a singleton
; Hashtable ket linecolumn

;;Return the first singleton among the first table's cell composed by a list (set)
;; which is not contained in the hashtable addressed by linecolumn values.
(define (find-singleton-set table visited-singleton-hashtable)
  (or-on-coordinate (lambda (line column cell)
                      
                        ;; TODO: better way to do it ?
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
      ;; TODO: avoid this verboseness
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
(solve sampletable3)
(solve sampletable2)
(solve sampletable)


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


