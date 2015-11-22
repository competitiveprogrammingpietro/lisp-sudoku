#lang racket/base
(require rackunit/text-ui)
(require rackunit
         "functional-programming-coursework.rkt")

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

(define compute-boxes-tests
  (test-suite "compute-boxes-tests"
              (test-case "Compute boxes from a 9x9 table"
                         (define list-input `(
                                              (11 12 13 14 15 16 17 18 19)
                                              (21 22 23 24 25 26 27 28 29)
                                              (31 32 33 34 35 36 37 38 39)
                                              (41 42 43 44 45 46 47 48 49)
                                              (51 52 53 54 55 56 57 58 59)
                                              (61 62 63 64 65 66 67 68 69)
                                              (71 72 73 74 75 76 77 78 79)
                                              (81 82 83 84 85 86 87 88 89)
                                              (91 92 93 94 95 96 97 98 99)
                                              ))
                          (define list-ouput `(
                                              (11 12 13 21 22 23 31 32 33)
                                              (14 15 16 24 25 26 34 35 36)
                                              (17 18 19 27 28 29 37 38 39)
                                              (41 42 43 51 52 53 61 62 63)
                                              (44 45 46 54 55 56 64 65 66)
                                              (47 48 49 57 58 59 67 68 69)
                                              (71 72 73 81 82 83 91 92 93)
                                              (74 75 76 84 85 86 94 95 96)
                                              (77 78 79 87 88 89 97 98 99)
                                              ))
                           (check-equal? (compute-boxes list-input 0) list-ouput "Compute boxes of 9x9 table"))))
                         
              
(define atom?-tests
  (test-suite "atom?-tests"
              (test-case "Test againt a pair"
                         (define pair-input (cons (list 1 2) (list 1 2)))
                         (check-equal? (atom? pair-input) #f "Negative test"))
              (test-case "Test again an atom"
                         (define atom-input 1)
                         (check-equal? (atom? atom-input) #t "Positive test"))
              ))

(define box-index-tests
  (test-suite "box-index"
              (test-case "Test one"
                         (check-equal? (box-index 0 8) 3)
                         (check-equal? (box-index 8 8) 8)
                         (check-equal? (box-index 4 4) 4))))


(define add-singleton-tests
  (test-suite "add-singleton-tests"
              (test-case "Add two singletons in the global list"
                         (define visited-singleton null)
                         (check-equal? (add-singleton 0 0 1 visited-singleton) `((0 0 1)) "Singleton NOT added to the global list"))))

(define is-singleton-present-tests
  (test-suite "is-singleton-present-tests"
              (test-case "Various tests to check the function works correctly"
                         (define visited-singleton `((0 0 1)))
                         (check-equal? (is-singleton-present 0 0 1 visited-singleton) #t "Singleton is already in the list")
                         (check-equal? (is-singleton-present 1 0 1 visited-singleton) #f "This singleton itsn't already in the list"))))

(define find-singleton-tests
  (test-suite "find-singleton"
              (test-case "Find the only singleton of a 3x3 table"
                         (define list-input `(
                                              ((1 2 3) 0 0)
                                              (0 (1 2 3) 1)
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
                           (define visited-singleton null)
                           (check-equal? (find-singleton list-input visited-singleton) `((1 2 1)))
                           )

              (test-case "No singleton present"
                         (define list-input `(
                                              ((1 2 3) 0 0)
                                              (0 (1 2 3) 0)
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
                           (define visited-singleton null)
                           (check-equal? (find-singleton list-input visited-singleton) #f))

              (test-case "Do not return twice the same singleton"
                         (define list-input `(
                                              ((1 2 3) 0 0)
                                              (0 (1 2 3) 1)
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
                           (define visited-singleton null)
                           (check-equal? (find-singleton list-input (find-singleton list-input visited-singleton)) #f))

              ))

(define remove-singleton-tests
  (test-suite "remove-singleton-tests"
              (test-case "Remove a present singleton"
                         (define list-input `(
                                              ((1 2 3) 0 0)
                                              (0 (1 2 3) 1)
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
                         (check-equal? (remove-singleton list-input 1 1) `(
                                                                           ((1 2 3) 0 0)
                                                                           (0 (2 3) 1)
                                                                           ((1 2 3) (1 2 3) (1 2 3))
                                                                           )))
              (test-case "Do not remove singletons present in other lines"
                         (define list-input `(
                                              ((1 2 3) 1 1)
                                              (0 (1 2 3) (1 2 3))
                                              ((1 2 3) 1 (1 2 3))
                                              ))
                         (check-equal? (remove-singleton list-input 1 1) `(
                                                                           ((1 2 3) 1 1)
                                                                           (0 (2 3) (2 3))
                                                                           ((1 2 3) 1 (1 2 3))
                                                                           )))))
                                       
                         
                                                     
(define visited-singleton null)
(define list-input `(
                                              (1 (1 2 3) 0)
                                              (0 (1 2 3) 1)
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
(find-singleton list-input null)
;(find-singleton list-input visited-singleton)
;(find-singleton list-input (find-singleton list-input visited-singleton))
;(find-singleton list-input (find-singleton list-input (find-singleton list-input visited-singleton)))

;; Run tests
;(run-tests transform-table-tests)
;(run-tests extract-tests)
;(run-tests compute-columns-tests)
;(run-tests compute-boxes-tests)
;(run-tests atom?-tests)
(run-tests find-singleton-tests)
;(run-tests add-singleton-tests)
;(run-tests is-singleton-present-tests)
(run-tests remove-singleton-tests)
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