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

;; Transform an entire table to
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



;; =================================================================================
;; FIND SINGLETON
;; Return a list (line-number column-number item)
;; Arguments :
;;             entry : table
;;             visited-singleton: list of already visited singleton in the format
;;                                ((line, column, number) ....)
;;         
;; =================================================================================

;; Check if a given singleton is already present in the global list
(define (is-singleton-present triple visited-singleton)
  (if (member triple visited-singleton) #t
      #f))
  
(define (find-singleton entry visited-singleton)
  (define (find-singleton-table-pvt table (indexl 1))
    (define (find-singleton-line-pvt line (indexc 1))
      (cond
        ([empty? line] #f)
        ([atom? (car line)]
         ;; Check if the just found number is already present in the singletons' list
         [let ([singleton-found (is-singleton-present (list indexl indexc (car line)) visited-singleton)])
           ;; New singleton discovered 
           (if (equal? singleton-found #f) 
               (list indexl indexc (car line))
               ;; This singleton had already been discovered
               (find-singleton-line-pvt (cdr line) (increment indexc)))])
        ;; No singleton found at this line
        (else (find-singleton-line-pvt (cdr line) (increment indexc)))))

    (if (empty? table) #f
        (let ([singleton-found-line (find-singleton-line-pvt (car table))])
          ;; No singleton (or no *new* singleton) found at this line, recursive call
          (if [equal? #f singleton-found-line]
              (find-singleton-table-pvt (cdr table) (increment indexl))
              ;; Return the new list of singletons, that is, the new singleton element
              ;; added at the beginning of the visited-singleton list
              (cons singleton-found-line visited-singleton)))))
  
  ;; Function entry point
  (find-singleton-table-pvt entry))
  

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
;; TODO: this where start from next time
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
;; UTILITY FUNCTION WORKING WITH COORDINATES
;; =================================================================================


;; For each table's cell  (func line column table[line*stride + column])
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

    ;(trace pvt-column)
    
    (if (null? table) #f
        (let ([result (pvt-column (car table))])
          (if (not result)
              (pvt-line (cdr table) (increment line-idx))
              result))))
    ;(trace pvt-line)
    ;(trace func)
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

;; TODO: unit test
(define (first-step-single table singleton-list)
  (let ([current-step-singleton-list (find-singleton table singleton-list)])
    (reduce table
            (get-number current-step-singleton-list)
            (get-line current-step-singleton-list)
            (get-column current-step-singleton-list))))  
         
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

;; =================================================================================
;; SECOND STEP
;; =================================================================================

;; Checks if there is a set other than the one specified by the coordinates (line-idx, column-idx)
;; containing a specific number, this function it is used to check if it is possible to reduce
;; a set to a singleton.
(define (is-present-other-sets line-idx column-idx table number)
  (let* ([start-line (+ (truncate (/ (- line-idx 1) 3)) 1)]
         [start-column (+ (* 3 (truncate (/ (- column-idx 1) 3))) 1)]
         [end-line (+ start-line 2)]
         [end-column (+ start-column 2)])
    
    ;; Body
    (or

     ;; Check for another set in this line containing the same number
     (or-on-line  (lambda (index cell)
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
                  line-idx)
     
     ;; Check for another set in this column containing the same number
     (or-on-column (lambda (index cell)
                     (if [or
                          (atom? cell)
                          (= index line-idx)
                          (= (count (lambda (item) (= item number)) cell) 0)]
                         #f
                         #t))
                   table
                   column-idx)
                     
     ;; Passing at the function the coordinated of the top-left corner of the box
     (or-on-coordinate (lambda (lindex cindex cell)
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
                       table))))
        
        
    
;; =================================================================================
;; SECOND STEP END
;; =================================================================================

;; MAIN TEST
;(define lines (transformTable sampletable))
;(first-step-single lines null)
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
 is-present-other-sets
 or-on-coordinate
 or-on-line
 or-on-column
 )


