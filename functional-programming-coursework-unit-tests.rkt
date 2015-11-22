#lang racket/base
(require rackunit/text-ui)
(require rackunit
         "functional-programming-coursework.rkt")

;; Test for the transformTable function
(define transform-table-tests
  (test-suite "transformTableTests"  
              (test-case "Nothing gets expanded when it is not required"
                          (define sampleTransformTableInputOne `((1 2 3 4 5 6 7 8 9)))
                          (define sampleTransformTableOuputOne `((1 2 3 4 5 6 7 8 9)))
                          (check-equal? (transformTable sampleTransformTableInputOne) sampleTransformTableOuputOne "Nothing to expand test transformTable"))
              (test-case "A correct expansions is performed when it's required"
                           (define sampleTransformTableInputTwo `(
                                                     (0 2 3 4 5 6 7 8 9)
                                                     (1 2 3 4 5 6 7 8 9)
                                                     ))
                           (define sampleTransformTableOuputTwo `(
                                                                  ((1 2 3 4 5 6 7 8 9) 2 3 4 5 6 7 8 9)
                                                                  (1 2 3 4 5 6 7 8 9)
                                                                  ))
                         (check-equal? (transformTable sampleTransformTableInputTwo) sampleTransformTableOuputTwo "Expanded incorrectly test transformTable"))))
(define extract-tests
  (test-suite "extract-tests"
              (test-case "Extract the first element"
                         (define test-list `(1 2 3 4 5 6 7 8 9))
                         (check-equal? (extract test-list 1) 1 ))

              (test-case "Extract the last element"
                         (define test-list `(1 2 3 4 5 6 7 8 9))
                         (check-equal? (extract test-list 9) 9 "Extract the last"))

              (test-case "Extract the the middle one"
                         (define test-list `(1 2 3))
                         (check-equal? (extract test-list 2) 2 "Extract the middle one"))))

(define compute-columns-tests
  (test-suite "compute-columns-tests"
              (test-case "Compute a simple table"
                         (define list-input `(
                                              (11 12 13)
                                              (21 22 23)
                                              (31 32 33)))
                         (define list-output `(
                                              (11 21 31)
                                              (12 22 32)
                                              (13 23 33)))
                         (check-equal? (compute-columns list-input 3) list-output "Compute columns of a simple 3x3 table"))))
                         
              
             

;; Run tests
(run-tests transform-table-tests)
(run-tests extract-tests)
(run-tests compute-columns-tests)
; This row represents all the possibilities, that is, all the numbers from 1 to 9
(define TBD `(1 2 3 4 5 6 7 8 9))

(define TEST `(
               ; 1
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 2
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 3
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 4
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) 9)
               ; 5
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 6
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 7
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 8
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
               ; 9
               ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
                (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))              
               ))