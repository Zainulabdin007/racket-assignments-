




;;extract-column is a function that takes a list and a index and returns the element at that index
;; from each list in the matrix

(define (extract-column lst in)
 (map (lambda (row) (index in row)) lst))

;; (index n lst) returns the n-th item from lst
;; index: Nat (listof Any) -> Any
;; Requires: 1 <= n <= (length lst)
(define (index n lst)
(first (foldl (lambda (x y) (rest y)) lst
(build-list (sub1 n) +))))




;;matrix-multiply creates a list where each element is a list of the dot products
;; between A's row and B's column. and this is done for each of A's rows with each of B's columns. 
;; Required : amount of columns in A equal the amount of rows in B
(define (matrix-multiply A B)
  (local [(define (dot lst0 lst1)
  (foldr + 0 (map * lst0 lst1)))]
  (map (lambda (row) (map (lambda (column) (dot row (extract-column B column)))
                           (build-list (length (first B)) (lambda (x) (add1 x))))) A)))



(define B (list (list 1 0) (list 0 1) (list 1 1)))
(check-expect (matrix-multiply A B)
(list (list 4 5)
      (list 10 11)
(list 16 17)
(list 22 23)))
(define C (list (list 1 1) (list 1 1)))
(define D (list (list 1 2) (list 3 4)))
(check-expect (matrix-multiply C D) (list (list 4 6) (list 4 6)))
(check-expect (matrix-multiply D C) (list (list 3 3) (list 7 7)))

;; 1x1
(define A1 '((5)))
(define B1 '((7)))
(check-expect (matrix-multiply A1 B1) '((35)))

;; 2x2
(define A2 '((1 2) (3 4)))
(define B2 '((5 6) (7 8)))
(check-expect (matrix-multiply A2 B2) '((19 22) (43 50)))

;; 2x3 * 3x2
(define A3 '((1 2 3) (4 5 6)))
(define B3 '((7 8) (9 10) (11 12)))
(check-expect (matrix-multiply A3 B3) '((58 64) (139 154)))

;; 3x2 * 2x3
(define A4 '((1 2) (3 4) (5 6)))
(define B4 '((7 8 9) (10 11 12)))
(check-expect (matrix-multiply A4 B4) '((27 30 33) (61 68 75) (95 106 117)))

;; identity matrix (2x2)
(define I2 '((1 0) (0 1)))
(define M2 '((7 8) (9 10)))
(check-expect (matrix-multiply I2 M2) M2)
(check-expect (matrix-multiply M2 I2) M2)

;; zero matrix
(define Z2 '((0 0) (0 0)))
(check-expect (matrix-multiply Z2 M2) Z2)
(check-expect (matrix-multiply M2 Z2) Z2)

;; row vector * column vector (1x3 * 3x1)
(define A5 '((1 2 3)))
(define B5 '((4) (5) (6)))
(check-expect (matrix-multiply A5 B5) '((32)))

;; column vector * row vector (3x1 * 1x3)
(define A6 '((1) (2) (3)))
(define B6 '((4 5 6)))
(check-expect (matrix-multiply A6 B6) '((4 5 6) (8 10 12) (12 15 18)))

;; fractions 2x2
(define Af '((1/2 1/3) (1/4 1/5)))
(define Bf '((3/4 2/3) (5/6 1/2)))
(check-expect (matrix-multiply Af Bf)
              '((47/72 1/2) (17/48 4/15)))


;; mixed positives and negatives
(define Am '((1 -2) (-3 4)))
(define Bm '((5 6) (7 8)))
(check-expect (matrix-multiply Am Bm) '((-9 -10) (13 14)))


;; larger 4x2 * 2x2
(define A7 '((1 2) (3 4) (5 6) (7 8)))
(define B7 '((1 1) (1 1)))
(check-expect (matrix-multiply A7 B7) '((3 3) (7 7) (11 11) (15 15)))




(define A (list (list 1 2 3) (list 4 5 6)
(list 7 8 9) (list 10 11 12)))

(check-expect (extract-column A 1) (list 1 4 7 10))
(check-expect (extract-column A 2) (list 2 5 8 11))


(check-expect (extract-column A 3) (list 3 6 9 12))

;; --- 1) Single row matrix ---
(check-expect (extract-column (list (list 10 20 30)) 1) (list 10))
(check-expect (extract-column (list (list 10 20 30)) 3) (list 30))

;; --- 2) Single column matrix ---
(check-expect (extract-column (list (list 'a)
                                    (list 'b)
                                    (list 'c)) 1)
              (list 'a 'b 'c))

;; --- 3) 1×1 matrix ---
(check-expect (extract-column (list (list 999)) 1)
              (list 999))

;; --- 4) Mixed data types ---
(check-expect (extract-column (list (list 1 "x" #t)
                                    (list 2 "y" #f)
                                    (list 3 "z" #t)) 2)
              (list "x" "y" "z"))

;; --- 5) Nested lists as elements  ---
(check-expect (extract-column (list (list '(a b) 10)
                                    (list '(c d) 20)) 1)
              (list '(a b) '(c d)))

;; --- 6) Repeating values ---
(check-expect (extract-column (list (list 5 5 5)
                                    (list 6 6 6)
                                    (list 7 7 7)) 2)
              (list 5 6 7))

;; --- 7) Larger matrix ---
(define big-matrix
  (list (list 1  2  3  4)
        (list 5  6  7  8)
        (list 9 10 11 12)
        (list 13 14 15 16)))

(check-expect (extract-column big-matrix 4)
              (list 4 8 12 16))

;; --- 8) Boundary column cases ---
(check-expect (extract-column big-matrix 1)
              (list 1 5 9 13))

(check-expect (extract-column big-matrix 3)
              (list 3 7 11 15))

;; --- 9) Uneven values but still rectangular ---
(define c (list (list -1 100)
                (list -2 200)
                (list -3 300)))

(check-expect (extract-column c 2) (list 100 200 300))
