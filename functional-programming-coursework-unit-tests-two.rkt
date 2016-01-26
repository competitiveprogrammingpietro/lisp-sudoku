#lang racket
(require rackunit/text-ui)
(require rackunit
         "functional-programming-coursework.rkt")
;; =================================================================================
;; REDUCE TESTS
;; =================================================================================

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


(define is-present-line-tests
  (test-suite "is-present-line-tests"
              (test-case "Singleton is not present in any other sets but the one whose coordinates are specified as argument"
                         (define list-input `(
                                              ((1 2 3) 2 (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2)) 
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)))
                           (check-equal? (is-present-line list-input 3 1 1) #f))

              (test-case "Singleton is present in a different sets in the same line"
                         (define list-input `(
                                              ((1 2 3) 2 (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2)) 
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)))
                           (check-equal? (is-present-line list-input 3 1 1) #t))


              ))
                           



(define is-present-column-tests
  (test-suite "is-present-column-tests"
              (test-case "Singleton not present in that column, present at another column instead"
                         (define list-input `(
                                              (1 (1 3) 3 4 5 6 7 8 9) 
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 (1 3) 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 2 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)))
                         (check-equal? (is-present-column list-input 3 1 2) #f))
              
              (test-case "Singleton present"
                         (define list-input `(
                                              (1 (1 3) 3 4 5 6 7 8 9) 
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 3) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 2 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)))
                         (check-equal? (is-present-column list-input 3 1 2) #t))
                         ))

(define is-present-box-tests
  (test-suite "is-present-box-tests"
              (test-case "Singleton not present in that box with the exception of the given one"
                         (define list-input `(
                                              (1 (1 3) 3 4 5 6 7 8 9) 
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 2 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)))
                         (check-equal? (is-present-box list-input 3 1 2) #f))
              
              (test-case "Singleton not present in that box with the exception of the given one, the one present at the botton right corner should not be considered"
                         (define list-input `(
                                              (1 (1 3) 3 4 5 6 7 8 9) 
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 2 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 (1 3))))
                         (check-equal? (is-present-box list-input 3 1 2) #f))

              (test-case "Singleton present in that box"
                         (define list-input `(
                                              (1 (1 3) 3 4 5 6 7 8 9) 
                                              (1 2 3 4 5 6 7 8 9)
                                              (1 (1 2) (1 3) 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 2 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)
                                              (1 (1 2) 3 4 5 6 7 8 9)))
                         (check-equal? (is-present-box list-input 3 1 2) #t))
                         ))

;; =================================================================================
;; SECOND STEP TESTS
;; =================================================================================
(define second-step-tests
  (test-suite "second-step-tests"
              (test-case "Another set with the number 1 other than the (1,1) cell is in the same line at (1, 3)"
                          (define list-input `(((1 2) 2 (1 2))
                                               ((3 4) (3 4) 3)
                                               ((3 4) (3 4) 8))
                            )
                          (check-equal? (is-present-other-sets 1 1 list-input 2) #t))
              
              (test-case "Another set with the number 1 other than the (1,1) cell is in the same column at (3, 1)"
                (define list-input `(
                                     ((1 2) 2 (3 4))
                                     ((3 4) (3 4) 3)
                                     ((1 4) (3 4) 8)
                                     )
                            )
                (check-equal? (is-present-other-sets 1 1 list-input 3) #t))
              (test-case "The set at (1, 1) is the only one to contains the number 1"
                (define list-input `(
                                     ((1 2) 2 (3 4))
                                     ((3 4) (3 4) 3)
                                     ((3 4) (3 4) 8)
                                     )
                            )
                (check-equal? (is-present-other-sets 1 1 list-input 1) #f))))


;; =================================================================================
;; SECOND TESTS END
;; =================================================================================

(run-tests second-step-tests)
;(run-tests is-present-box-tests)
;(run-tests is-present-line-tests)
;(run-tests reduce-tests)
;(run-tests is-present-column-tests)
