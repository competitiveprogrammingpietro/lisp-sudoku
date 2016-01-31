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


;;; =============== RUBBISH ?
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
;================= END RUBBISH

;; Utility functions

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
;; UTILITY FUNCTION FOR OPERATING ON COORDINATES
;; =================================================================================

;; For each table's cell call the func table with the following parameter
;; (func line column table[line*stride + column])
(define (operate-on-coordinates-till-true func table)
  
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

;    (trace pvt-column)
    
    (if (null? table) #f
        (let ([result (pvt-column (car table))])
          (if (not result)
              (pvt-line (cdr table) (increment line-idx))
              result))))
   ; (trace pvt-line)
  ;  (trace func)
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
  (define (is-present-other-sets-line line (column 1))
    (cond
      ([null? line] #f)

      ;; Recursive step if
      ;; 1) the item is an atom
      ;; 2) the column matches the index of the one being under check
      ;; 3) set does not contain the number i.e count = 0
      ([or (atom? (car line))
           (= column column-idx)
           (= (count (lambda (item) (= item number)) (car line)) 0)]
       (is-present-other-sets-line (cdr line) (increment column)))

      ;; True otherwise
      (else #t)))

  (define (is-present-other-sets-column table (line 1))
    (cond
      ([null? table] #f)
      (else

       ;; Bound item to the line's column
       (let ((item (list-ref (car table) (- column-idx 1))))
         (cond
           
           ;; 1) The line's item is an atom
           ;; 2) This line's column has not to be considered
           ;; 3) This line's column set does not contain the number
           ;; If one of them is true make the recursive call
           ([or (atom? item)
                (= line line-idx)
                (= (count (lambda (x) (= x number)) item) 0)]
            (is-present-other-sets-column (cdr table) (increment line)))

           ;; Otherwise the number was present in the line's column's set
           (else #t))))))

  
  (define (is-present-other-sets-box reducted-table start-line start-column (accx 1))
    (define (is-present-other-sets-box-line reducted-table-line (accy 1))
      (cond

        ;; End and not found
        ([null? reducted-table-line] #f)

        ;; Same one or not found, recursive step
        ([or
          (and (= accy column-idx) (= line-idx accx))
          (not (member number (car reducted-table-line)))]
         (is-present-other-sets-box-line (cdr reducted-table-line) (increment accy)))

        ;; We have found it
        (else
         #t)))

;    (trace is-present-other-sets-box-line)
    (cond
      ([null? reducted-table] #f)
      
       ;; element is not a set then recursive case
       ([atom? (car reducted-table)]
        (is-present-other-sets-box (cdr reducted-table) start-line start-column (increment accx)))

       ;; Check if we have found it
       ([is-present-other-sets-box-line (car reducted-table)]
         #t)

       ;; Recursive step again
       (else
        (is-present-other-sets-box (cdr reducted-table) start-line start-column (increment accx)))
       ))
;  (trace is-present-other-sets-line)
;  (trace is-present-other-sets-column)
 ; (trace is-present-other-sets-box)
  ;; Test against the line and column
  (let*
      
      ;; Bindings
      (
       [start-line (truncate (/ (- line-idx 1) 3))]
       [start-column (* 3 (truncate (/ (- column-idx 1) 3)))]

       ;; Build a list representing the box
       [reducted-table-lines (take (drop table start-line) 3)]
       [reducted-table-line-one (take (drop (car reducted-table-lines) start-column) 3)]
       [reducted-table-line-two (take (drop (car (cdr reducted-table-lines)) start-column) 3)]
       [reducted-table-line-three (take (drop (car (cdr (cdr reducted-table-lines))) start-column) 3)]
       [reducted-table (list reducted-table-line-one reducted-table-line-two reducted-table-line-three)]
       )
    
    ;; Body
    (or
     (is-present-other-sets-line (list-ref table (- line-idx 1)))
     (is-present-other-sets-column table)

     ;; Passing at the function the coordinated of the top-left corner of the box
     (is-present-other-sets-box reducted-table (* start-line 3) (* start-column 3)))
    ))


;; The format of the set-singleton-list is;
;; (line, column, (list of set's number already examined)
#|
(define (find-set-singleton table set-singleton-list)
  
  
  (define (find-set-singleton-pvt line column item)

    ;; We only consider sets
    (when (atom? item)
      #f)

    ;; Make sure this set hasn't been already througly examined 
    (when (ormap
           (lambda (x)
             ((if (and (= (list-ref x 0) line)
                       (= (cdr x) column)
                       
                       )

  |#                
                        
    
    
        
    
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
 operate-on-coordinates-till-true
 )


