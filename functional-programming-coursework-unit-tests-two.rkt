#lang racket
(require rackunit/text-ui)
(require rackunit
         "functional-programming-coursework.rkt")
(define reduce-tests
  (test-suite "reduce-tests"
              (test-case "Reduce a table where there is nothing to do, indentity function expected"
                          (define list-input `(
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               ))
                          (check-equal? (reduce list-input 2 2 2) list-input))
              
              (test-case "Reduce where a singleton is present at line 1 column 2 with value 2"
                          (define list-input `(
                                               ((1 2) 2 5 (1 2) (1 2) 1 (1 2) (1 2) (1 2))
                                               (1 (1 2) 4 2 5 (1 2) (1 2) (1 2) (1 2))
                                               ((1 2) (1 2) 6 (1 2) (1 2) 4 2 1 (1 2) (1 2) (1 2))
                                               (1 (1 2) 3 4 5 6 7 8 9)
                                               (1 (1 2) 3 4 5 6 7 8 9)
                                               (1 (1 2) 3 4 5 6 7 8 9)
                                               (1 (1 2) 3 4 5 6 7 8 9)
                                               (1 (1 2) 3 4 5 6 7 8 9)
                                               (1 (1 2) 3 4 5 6 7 8 9)
                                               ))
                          (define list-output `(
                                                ((1) 2 5 (1) (1) 1 (1) (1) (1))
                                                (1 (1) 4 2 5 (1 2) (1 2) (1 2) (1 2))
                                                ((1) (1) 6 (1 2) (1 2) 4 2 1 (1 2) (1 2) (1 2))
                                                (1 (1) 3 4 5 6 7 8 9)
                                                (1 (1) 3 4 5 6 7 8 9)
                                                (1 (1) 3 4 5 6 7 8 9)
                                                (1 (1) 3 4 5 6 7 8 9)
                                                (1 (1) 3 4 5 6 7 8 9)
                                                (1 (1) 3 4 5 6 7 8 9)         
                                               ))
                          (check-equal? (reduce list-input 2 1 2) list-output))
              ))

(define is-present-other-set-tests
  (test-suite "is-present-other-set-tests"
              
              (test-case "Check line: another set with the number 3 other than the (6,3) cell is at the same line at (6, 7)"
                (define list-input `(
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (3 4) (1 2) (1 2) (1 2) (3 4) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     )
                  )
                (check-equal? (is-present-other-sets 6 3 3 list-input) #t))
              
              (test-case "Check column: another set with the number 3 other than the (1,3) cell is at the same column at (3, 3)"
                (define list-input `(
                                     ((1 2) (1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     
                                     )
                  )
                (check-equal? (is-present-other-sets 1 3 3 list-input) #t))

              (test-case "Box check: another set with the number 3 other than the (1,3) cell is present in the same box at (3, 2)"
                (define list-input `(
                                     ((1 2) (1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     )
                            )
                (check-equal? (is-present-other-sets 1 3 3 list-input) #t))
              
              (test-case "Box check: another set with the number 3 other than the (1,3) cell is present but in a different box at (8, 2) (9 6)"

                (define list-input `(
                                     ((1 2) (1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
              )
                            )
                (check-equal? (is-present-other-sets 1 3 3 list-input) #f))
              
              (test-case "Box check: another set with the number 3 other than the (1,3) cell is present in the same box at (3, 1) others are in different boxes (8, 2) (9 6)"

                (define list-input `(
                                     ((1 2) (1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     )
                            )
                (check-equal? (is-present-other-sets 1 3 3 list-input) #t))
              
              (test-case "The set at (1, 1) is the only one to contains the number 3"
                (define list-input `(
                                     ((3 4) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     )
                            )

                (check-equal? (is-present-other-sets 1 1 3 list-input) #f))
              
              
              ))

(define find-singleton-test
  (test-suite "find-singleton-test"
              (test-case "Return the first singleton, which is the number 1 at 1,2"
                (define list-input `(
                                     (1 (1 2 3 4 5) 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     ))
                (check-equal? (find-singleton-set list-input (make-hash)) `(1 2 1)))

              (test-case "Return the first valid singleton, which is the number 2 at 1,2"
                (define list-input `(
                                     (1 (1 2 3 4 5) 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     ))
                (define ht (make-hash))
                (hash-set! ht "12" `(1))
                (check-equal? (find-singleton-set list-input ht) `(1 2 2)))

              (test-case "Return the first valid singleton, which is the number 8 at 9,2. The hashtable contains several key, value pairs"
                (define list-input `(
                                     (1 (1 2 3 4 5) 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 (1 2 3 4) 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 (8 9) 3 4 5 6 7 8 9)
                                     ))
                (define ht (make-hash))
                (hash-set! ht "12" `(1 2 3 4 5))
                (hash-set! ht "76" `(1 2 3 4))
                (check-equal? (find-singleton-set list-input ht) `(9 2 8)))

              (test-case "Return false, since there are not singleton"
                (define list-input `(
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     ))
                (define ht (make-hash))
                (hash-set! ht "12" `(1))
                (check-equal? (find-singleton-set list-input ht) #f))

              ))
(define reduce-set-tests
  (test-suite "reduce-set-tests"
              (test-case "Reduce set at 1,2 to be 2"
                (define list-input `(
                                     (1 (1 2 3 4 5) 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     ))
                (define list-output `(
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     (1 2 3 4 5 6 7 8 9)
                                     ))
                
                (check-equal? (reduce-set 1 2 2 list-input) list-output)
                )))
;; =================================================================================
;; SECOND TESTS END
;; =================================================================================


;(run-tests reduce-tests)
;(run-tests find-singleton-test)
;(run-tests is-present-other-set-tests)
(run-tests reduce-set-tests)
