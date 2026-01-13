;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Zain Bughio (21199539)
;; CS 135 Fall 2025
;; Assignment 09, Problem 2
;; ***************************************************
;;


;;
;;question 2, part a
;;
;;
;; the function super-filter is a function that works like a filter but works on nested lists as well
;;
(define (super-filter pred? lst)
  (cond [(empty? lst) empty]
        [(list? (first lst))
         (cons (super-filter pred? (first lst)) (super-filter pred? (rest lst)))]
        [(pred? (first lst))
         (cons (first lst) (super-filter pred? (rest lst)))]
        [else (super-filter pred? (rest lst))]))

;;
;;question 2, part b
;;
;;
;; the function  ruthless takes out all the 'ruth symbols out of the given list
;; Required : the given list must be list of only symbols
(define (ruthless lst)
  (super-filter (lambda (x) (not (symbol=? x 'ruth ))) lst))

;;
;;question 2, part c
;;
;; takes a list and removes all the natural numbers less then the given n
;; Required : the given list must only contain Nat numbers
(define (supersize n lst)
  (super-filter (lambda (x) (<= n x)) lst))

;;
;;question 2, part d
;;
;; the function will just keep the elements that are against the predicate
;;
(define (super-keeper pred? lst)
  (super-filter (lambda (x) (not (pred? x))) lst))

(check-expect
(super-keeper
odd?
(list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
(list (list 2 (list 2 4) 6 (list 8)) 10 12))


(check-expect (supersize 4 (list 8 1 (list 2 6 3) 10 1))
(list 8 (list 6) 10))
(check-expect (supersize 200 (list 8 5 (list 2 6 3) 10 1))
(list empty))
(check-expect (supersize 200 (list 5 6 7)) empty)


(define M (list 'ruth 'chris 'steakhouse
(list 'ruth 'handler 'created 'what
(list 'babe 'ruth 'blue)
(list 'ruth 'ruth)
'ruth 'bader 'ginsberg)
'hello))
(check-expect (ruthless M)
(list 'chris 'steakhouse
(list 'handler 'created 'what
(list 'babe 'blue)
empty
'bader 'ginsberg)
'hello))


;; Basic single element
(check-expect (ruthless '(ruth)) '())
(check-expect (ruthless '(a)) '(a))

;; Top-level list with mixed symbols
(check-expect (ruthless '(ruth a b)) '(a b))
(check-expect (ruthless '(x y ruth)) '(x y))

;; Nested lists
(check-expect (ruthless '(a (ruth b) c)) '(a (b) c))
(check-expect (ruthless '((ruth))) '(()))
(check-expect (ruthless '((a ruth) (ruth b))) '((a) (b)))

;; Deeply nested
(check-expect (ruthless '(a (b (ruth c) d) e)) '(a (b (c) d) e))
(check-expect (ruthless '(((ruth)))) '((())))
(check-expect (ruthless '((ruth (ruth (ruth x))))) '((( (x)))))

;; Multiple occurrences
(check-expect (ruthless '(ruth ruth ruth)) '())
(check-expect (ruthless '(a ruth b ruth c)) '(a b c))

;; Empty list handling
(check-expect (ruthless '()) '())
(check-expect (ruthless '(ruth ())) '(()) )


;; Complex nested structure
(check-expect (ruthless '(a (ruth (b ruth (c ruth))) d)) '(a ((b (c))) d))
(check-expect (ruthless '((ruth a) (b (ruth c) d) ruth)) '((a) (b (c) d)))
(check-expect (ruthless '(ruth (ruth (ruth (ruth a))) b)) '((((a))) b))

;; Edge cases with empty lists inside
(check-expect (ruthless '(ruth () (ruth ()))) '(() (())))
(check-expect (ruthless '((ruth) ())) '(() ()))

;; Single-level list with no 'ruth
(check-expect (ruthless '(a b c d e f)) '(a b c d e f))

;; Mixed symbols and nested empty lists
(check-expect (ruthless '(ruth () (x))) '(() (x)))
(check-expect (ruthless '(ruth ())) '(()))
(check-expect (ruthless '(() ruth)) '(()))


(define L (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
(check-expect (super-filter odd? L)
(list 1 (list (list 3) 5 (list 7 9)) 11))

(check-expect (super-filter (lambda (x) (>= x 9)) L)
(list (list empty (list 9)) 10 11 12))
(check-expect (super-filter (lambda (x) (>= x 11)) L)
(list (list empty empty) 11 12))


;; numbers and integers
(check-expect (super-filter number? '(1 2 3 (4 5) x "a")) '(1 2 3 (4 5) ))
(check-expect (super-filter integer? '(1 2 3 4 5.5 "hi" x)) '(1 2 3 4))
(check-expect (super-filter even? '(1 2 3 4 5 (6 7))) '( 2  4 (6)))
(check-expect (super-filter odd? '(1 2 3 4 5 (6 7))) '(1  3  5 ( 7)))

;; zero?
(check-expect (super-filter zero? '(0 1 0 2 (0 3))) '(0  0  (0 )))

;; relational predicates (<, >, <=, >=)
(check-expect (super-filter (lambda (x) (< x 3)) '(1 2 3 4 (0 5))) '(1 2  (0 )))
(check-expect (super-filter (lambda (x) (> x 2)) '(1 2 3 4 (0 5))) '(  3 4 ( 5)))
(check-expect (super-filter (lambda (x) (<= x 2)) '(1 2 3 0 (5 2))) '(1 2  0 ( 2)))
(check-expect (super-filter (lambda (x) (>= x 3)) '(1 2 3 4 (0 5))) '(3 4 ( 5)))

;; strings and chars
(check-expect (super-filter string? '("a" "bc" 1 (2 "d"))) '("a" "bc"  ( "d")))
(check-expect (super-filter char? '(#\a #\b "c" 1 (#\d "e"))) '(#\a #\b (#\d )))


;; deeply nested numbers
(check-expect (super-filter number? '(1 (2 (3 (4 5) 6) 7) 8)) '(1 (2 (3 (4 5) 6) 7) 8))

;; all fail
(check-expect (super-filter zero? '(1 2 3 (4 5))) '(   ( )))

;; all pass
(check-expect (super-filter number? '(1 2 3 (4 5))) '(1 2 3 (4 5)))

;; empty list
(check-expect (super-filter number? '()) '())

;; single-element lists
(check-expect (super-filter integer? '((1))) '((1)))
(check-expect (super-filter string? '(("hi"))) '(("hi")))

;; nested empty lists
(check-expect (super-filter number? '(() ())) '(() ()))

;; Basic single element
(check-expect (supersize 5 '(3)) '())
(check-expect (supersize 5 '(5)) '(5))
(check-expect (supersize 5 '(7)) '(7))

;; Top-level list with mixed numbers
(check-expect (supersize 5 '(1 5 7 3 10)) '(5 7 10))
(check-expect (supersize 10 '(1 2 3 4 5)) '())
(check-expect (supersize 0 '(1 2 3)) '(1 2 3))

;; Nested lists
(check-expect (supersize 5 '(3 (5 6) 2)) '((5 6)))
(check-expect (supersize 7 '((1 2) (7 8) 6)) '(() (7 8) ))
(check-expect (supersize 3 '(1 (2 (3 4) 1) 5)) '( ( (3 4) ) 5))

;; Deeply nested
(check-expect (supersize 5 '(1 (2 (3 (5 6) 4) 2) 7)) '( ( ( (5 6) ) ) 7))
(check-expect (supersize 0 '((0) (0 (1)) 2)) '((0) (0 (1)) 2))

;; Multiple occurrences
(check-expect (supersize 5 '(5 5 5)) '(5 5 5))
(check-expect (supersize 6 '(5 6 7)) '(6 7))

;; Empty list handling
(check-expect (supersize 5 '()) '())
(check-expect (supersize 5 '(5 () 6)) '(5 () 6))



;; Edge cases with nested empty lists
(check-expect (supersize 5 '( () () )) '(() ()))
(check-expect (supersize 5 '(5 () 6 () 4)) '(5 () 6 ()))

;; Single-level list with no numbers >= n
(check-expect (supersize 10 '(1 2 3 4 5)) '())

;; Single-level list all numbers >= n
(check-expect (supersize 2 '(2 3 4 5 6)) '(2 3 4 5 6))

;; Deep nesting with all below n
(check-expect (supersize 10 '(1 (2 (3 (4 5))))) '( ( ( ( )))))

;; Deep nesting with mixed numbers
(check-expect (supersize 3 '(1 (2 (3 (4 5) 1) 0) 6)) '( ( (3 (4 5) ) ) 6))


;; Basic single element
(check-expect (super-keeper zero? '(0)) '())
(check-expect (super-keeper zero? '(1)) '(1))

;; Top-level list
(check-expect (super-keeper even? '(1 2 3 4 5)) '(1 3 5))
(check-expect (super-keeper odd? '(1 2 3 4 5)) '(2 4))
(check-expect (super-keeper number? '(1 "a" 2 'b)) '("a" 'b))
(check-expect (super-keeper string? '(1 "a" "b" 2)) '(1 2))
(check-expect (super-keeper symbol? '(a b "c" 1)) '("c" 1))

;; Nested lists
(check-expect (super-keeper zero? '(1 (0 2) 3)) '(1 ( 2) 3))
(check-expect (super-keeper even? '(1 (2 3 4) 5)) '(1 ( 3 ) 5))
(check-expect (super-keeper odd? '(1 (2 3 4) 5)) '( (2 4) ))

;; Deeply nested
(check-expect (super-keeper zero? '(0 (0 (0 1 2) 0) 3)) '( ( ( 1 2 ) ) 3))
(check-expect (super-keeper even? '(1 (2 (3 4) 5) 6)) '(1 ( (3  ) 5) ))

;; Multiple occurrences
(check-expect (super-keeper zero? '(0 0 0)) '())
(check-expect (super-keeper odd? '(1 3 5)) '())

;; Empty list handling
(check-expect (super-keeper zero? '()) '())
(check-expect (super-keeper odd? '(() 1 2)) '(() 2))
(check-expect (super-keeper even? '(() 1 2)) '(() 1))

;; Mixed types
(check-expect (super-keeper number? '(1 "a" b 2)) '("a" b))
(check-expect (super-keeper string? '(1 "a" b 2)) '(1 b 2))
(check-expect (super-keeper symbol? '(1 "a" hi 2)) '(1 "a" 2))

;; Nested empty lists
(check-expect (super-keeper zero? '( () (() ()) )) '(() (() ())))
(check-expect (super-keeper odd? '( () (() ()) )) '(() (() ())))

;; Predicates with comparisons
(check-expect (super-keeper (lambda (x) (< x 3)) '(1 2 3 4)) '(3 4))
(check-expect (super-keeper (lambda (x) (>= x 3)) '(1 2 3 4)) '(1 2))
(check-expect (super-keeper (lambda (x) (> x 2)) '(1 2 3 4)) '(1 2))
(check-expect (super-keeper (lambda (x) (<= x 2)) '(1 2 3 4)) '(3 4))

;; Deep nesting with mixed numbers
(check-expect (super-keeper even? '(1 (2 (3 (4 5) 1) 0) 6)) '(1 ( (3 ( 5) 1) ) ))
(check-expect (super-keeper odd? '(1 (2 (3 (4 5) 1) 0) 6)) '( (2 ( (4 ) ) 0) 6))

;; Single-level no matching atoms
(check-expect (super-keeper even? '(1 3 5 7)) '(1 3 5 7))
(check-expect (super-keeper odd? '(2 4 6)) '(2 4 6))
