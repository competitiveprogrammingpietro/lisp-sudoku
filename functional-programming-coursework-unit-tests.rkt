#lang racket/base
(require racket/trace)
(require rackunit/text-ui)
(require rackunit
         "functional-programming-coursework.rkt")

(define transform-table-tests
  (test-suite "transformTableTests"  

              (test-case "Nothing gets expanded when it is not required"
                          (define sampleTransformTableInputOne `((1 2 3 4 5 6 7 8 9)))
                          (define sampleTransformTableOuputOne `((1 2 3 4 5 6 7 8 9)))
                          (check-equal? (transformTable sampleTransformTableInputOne)
                                        sampleTransformTableOuputOne
                                        "Nothing to expand test transformTable"))
              
              (test-case "A correct expansions is performed when it's required"
                           (define sampleTransformTableInputTwo `(
                                                                  (0 2 3 4 5 6 7 8 9)
                                                                  (1 2 3 4 5 6 7 8 9)
                                                                  ))
                           (define sampleTransformTableOuputTwo `(
                                                                  ((1 2 3 4 5 6 7 8 9) 2 3 4 5 6 7 8 9)
                                                                  (1 2 3 4 5 6 7 8 9)
                                                                  ))
                           (check-equal? (transformTable sampleTransformTableInputTwo)
                                         sampleTransformTableOuputTwo
                                         "Expanded incorrectly test transformTable"))))

(define increment-tests
  (test-suite "increment-tests"
              
              (test-case "Single increment"
                (check-equal? (increment 1) 2))
              
              (test-case "Double increment"
                (check-equal? (increment (increment 1)) 3))))

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
                (check-equal? (box-index 1 1) 1))
              (test-case "Test two"
                (check-equal? (box-index 1 9) 3))
              (test-case "Test three"  
                (check-equal? (box-index 5 5) 5))
              (test-case "Test four"
                         (check-equal? (box-index 9 9) 9))))


(define add-singleton-tests
  (test-suite "add-singleton-tests"
              (test-case "Add two singletons in the global list"
                         (define visited-singleton null)
                         (check-equal? (add-singleton 0 0 1 visited-singleton) `((0 0 1)) "Singleton NOT added to the global list"))))

(define remove-singleton-list-tests
  (test-suite "remove-singleton-list-tests"
              (test-case "Test 1"
                         (define test-list-one `((1 2 3)))
                         (define test-list-two 1)
                         (check-equal? (remove-singleton-element (car test-list-one) 3) `(1 2))
                         (check-equal? (remove-singleton-element test-list-two 3) 1))))


(define remove-singleton-column-tests
  (test-suite "remove-singleton-column-tests"
              (test-case "No singleton to remove"
                         (define test-list-one `(1 2 3 4 5 6 7 8 9))
                         (check-equal? (remove-singleton-column test-list-one 2 3) test-list-one))
              (test-case "Remove singleton"
                         (define test-list-one `(1 (3 4 5) 2 3))
                         (define result-list-one `(1 (4 5) 2 3))
                         (check-equal? (remove-singleton-column test-list-one 2 3) result-list-one))
              (test-case "No singleton to remove but sublists present"
                         (define test-list-one `(1 (3 4 5) (1 2 3 4) 2 3))
                         (define result-list-one `(1 (3 4 5) (1 2 3 4) 2 3))
                         (check-equal? (remove-singleton-column test-list-one 2 9) result-list-one))))

(define remove-singleton-table-column-tests
  (test-suite "remove-singleton-table-column-tests"
              (test-case "No singleton to remove because no sublist"
                         (define test-list-input `(
                                                   ;; Line 1..n
                                                   (1 2 3 4 5 6 7 8 9)
                                                   (1 2 3 4 5 6 7 8 9)))
                         (check-equal? (remove-singleton-table-column test-list-input 2 3) test-list-input))

              (test-case "Singleton remove successfully at column 2"
                         (define test-list-input `(
                                                   ;; Line 1..n
                                                   (1 (1 2 3 4 5) 3 4 5 6 7 8 9)
                                                   (1 (1 2 3 4 5) 2 3 4 5 6 7 8 9)))
                         (define test-list-output `(
                                                   ;; Line 1..n
                                                   (1 (1 2 4 5) 3 4 5 6 7 8 9)
                                                   (1 (1 2 4 5) 2 3 4 5 6 7 8 9)))
                         (check-equal? (remove-singleton-table-column test-list-input 2 3) test-list-output))
              
         
              ))

(define remove-singleton-table-line-tests
  (test-suite "remove-singleton-table-line-tests"
              (test-case "No singleton to remove because no sublist"
                         (define test-list-input `(
                                                   ;; Line 1..n
                                                   (1 2 3 4 5 6 7 8 9)
                                                   (1 2 3 4 5 6 7 8 9)))
                         (check-equal? (remove-singleton-table-line test-list-input 2 3) test-list-input))

              (test-case "Singletons to remove at line two and three but just the third line gets affected"
                         (define test-list-input `(
                                                   ;; Line 1..n
                                                   (1 2 3 4 5 6 7 8 9)
                                                   (1 (1 2 3 4) 2 3 4 5 6 7 8 9)
                                                   (1 (1 2 3 4) 3 4 5 6 7 8 9)
                                                   ))
                         (define test-list-output `(
                                                   ;; Line 1..n
                                                   (1 2 3 4 5 6 7 8 9)
                                                   (1 (1 2 3 4) 2 3 4 5 6 7 8 9)
                                                   (1 (1 2 4) 3 4 5 6 7 8 9)
                                                   ))
                         (check-equal? (remove-singleton-table-line test-list-input 3 3) test-list-output))

               (test-case "Sublists present but no singleton to remove"
                         (define test-list-input `(
                                                   ;; Line 1..n
                                                   (1 2 3 4 5 6 7 8 9)
                                                   (1 (1 2) 2 3 4 5 6 7 8 9)
                                                   (1 (1 2) 3 4 5 6 7 8 9)
                                                   ))
                         (check-equal? (remove-singleton-table-line test-list-input 3 3) test-list-input))

              ))


(define find-singleton-tests
  (test-suite "find-singleton"
              
              (test-case "Find the only singleton of a 3x3 table"
                         (define list-input `(
                                              ((1 2 3) (1 2 3)  (1 2 3))
                                              ((1 2 3) (1 2 3) 1)
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
                           (define visited-singleton null)
                         (check-equal? (find-singleton list-input visited-singleton) `((2 3 1))))
              (test-case "No singleton present"
                         (define list-input `(
                                              ((1 2 3) (1 2 3) (1 2))
                                              ((1 2) (1 2 3) (1 2))
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
                         (define visited-singleton null)
                         (check-equal? (find-singleton list-input visited-singleton) #f))

              (test-case "Do not return twice the same singleton"
                         (define list-input `(
                                              ((1 2 3) (1 2) (1 2))
                                              ((1 2 3) (1 2 3) 1)
                                              ((1 2 3) (1 2 3) (1 2 3))
                                              ))
                         (define visited-singleton null)
                         (check-equal? (find-singleton list-input (find-singleton list-input visited-singleton)) #f))

              (test-case "Return all the singleton in the right order"
                         (define list-input `(
                                              (5 1 (1 2))
                                              ((1 2 3) (1 2 3) 2)
                                              (3 (1 2 3) (1 2 3))
                                              ))
                         (define visited-singleton null)
                         (check-equal?
                          (find-singleton list-input
                                          (find-singleton list-input
                                                          (find-singleton list-input
                                                                          (find-singleton list-input visited-singleton))))
                          `((3 1 3) (2 3 2) (1 2 1) (1 1 5))))
              
              ))
             
(define remove-singleton-table-box-tests
  (test-suite "remove-singleton-table-box-tests"
               (test-case "No sigleton to remove"
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
                            (check-equal? (remove-singleton-table-box list-input 2 2) list-input))

               (test-case "Singleton to remove at box one"
                          (define list-input `(
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               ))

                          (define list-output `(
                                               (1 (1 2) 2 3 4 5 6 7 8 9)
                                               (1 (1 2) 2 3 4 5 6 7 8 9)
                                               (1 (1 2) 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               ))
                            (check-equal? (remove-singleton-table-box list-input 1 3) list-output))

               (test-case "Singleton to remove at box one, but box two asked instead therefore nothing is changed"
                          (define list-input `(
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               ))

                          (define list-output `(
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 (1 2 3) 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               (1 2 3 4 5 6 7 8 9)
                                               ))
                          (check-equal? (remove-singleton-table-box list-input 2 3) list-output))
               ))
                                       
(define get-tests
  (test-suite "get-tests"
              (test-case "Get a line"
                         (define list-input `((1 2 3)))
                         (check-equal? (get-number list-input) 3)
                         (check-equal? (get-column list-input) 2)
                         (check-equal? (get-line list-input) 1))))

(define or-on-coordinate-tests
  (test-suite "operate-on-coordinates-till-true"
              (test-case "Return the first number equals to three which is at (9,9)"
                (define list-input `(
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 3)
                                     ))
                (define (custom-func line column number)
                  (if (= number 3)
                      (cons line column)
                      #f))
                (check-equal? (or-on-coordinate custom-func list-input) `(9 . 9)))))

(define or-on-line-tests
  (test-suite "or-on-coordinates-line"
              (test-case "Return the first number equals to three at line 4th which is at (4 . 5)"
                (define list-input `(
                                     (0 0 1 0 0 0 0 5 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 5 0 0 0 0) ;; Wanted line
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 5 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 0)
                                     (0 0 0 0 0 0 0 0 3)
                                     ))
                (define (custom-func column number)
                  (if (= number 5)
                      (cons 4 column)
                      #f))
                (check-equal? (or-on-line custom-func list-input 4) `(4 . 5)))))

(define or-on-column-tests
  (test-suite "or-on-coordinates-line"
              (test-case "Return the first number equals to three at column 7th which is at (4 . 7)"
                (define list-input `(
                                     (1 0 0 0 0 0 0 0 0)
                                     (2 0 0 0 0 0 0 0 0)
                                     (3 0 0 0 0 0 0 0 0)
                                     (4 0 0 0 5 0 5 0 0)
                                     (5 0 0 0 3 0 0 0 0)
                                     (6 0 0 0 0 0 0 0 0)
                                     (7 0 0 0 0 0 0 0 0)
                                     (8 0 0 0 0 0 0 0 0)
                                     (9 0 0 0 0 0 0 0 3)
                                     ))
                (define (custom-func line item)
                  (if (= item 5)
                      (cons line 7)
                      #f))
                (check-equal? (or-on-column custom-func list-input 7) `(4 . 7)))))

(define first-step-tests
  (test-suite "first-step-tests"
              (test-case "A set reducted to a singleton is considered in the following reduction"
                (define list-input `(
                                     ((1 2) (1 2 3) (1 2) (5 6)   5   (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (6 7) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                     ))

                (define list-output `(
                                      ((1 2) (1 2 3) (1 2)   6   5   (1 2) (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2)   7 (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                      ((1 2) (1 2 3) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
                                      ))
                (check-equal? (first-step list-input (find-singleton list-input null)) list-output))
              ))
                

;; Generic tests
(run-tests box-index-tests)
(run-tests transform-table-tests)
(run-tests increment-tests)
(run-tests atom?-tests)
(run-tests or-on-coordinate-tests)
(run-tests or-on-line-tests)
(run-tests or-on-column-tests)

;; Algorithm's first step tests
(run-tests find-singleton-tests)
(run-tests remove-singleton-list-tests)
(run-tests remove-singleton-column-tests)
(run-tests remove-singleton-table-column-tests)
(run-tests remove-singleton-table-line-tests)
(run-tests remove-singleton-table-box-tests)
(run-tests add-singleton-tests)
(run-tests  get-tests)
(run-tests first-step-tests)


