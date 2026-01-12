
;; prime? is a function that takes a number returns true is something is a prime number and
;; false otherwise
;;
(define (prime? n)
  (and (> n 1)
  (empty? (filter (lambda (divisor) (= (remainder n divisor) 0)) (build-list (max 0 (- n 2))
                                                                             (lambda (x) (+ x 2)))))))


;;list-primes is a function that lists all the primes from 2 to a given n

(define (list-primes n)
  (filter prime? (build-list (max 0 (- n 1)) (lambda (x) (+ x 2)))))


;; gb is a function that produces a list of pairs(x,y) that when summed, will equal the given natural
;; nubmer n where n>=4 and x<=y and x and y are both prime numbers
(define (gb n)
  (filter (lambda (pair) (= (+ (first pair) (second pair)) n))
          (foldl append empty (map (lambda (x) (map (lambda (y) (list x y))
          (filter (lambda (y) (>= y x)) (list-primes n)))) (list-primes n)))))


(check-expect (gb 4) (list (list 2 2)))
(check-expect (gb 10) (list (list 5 5) (list 3 7)))
(check-expect (gb 9) (list (list 2 7)))
(check-expect (gb 11) empty)
;; Smallest allowed input
(check-expect (gb 4) (list (list 2 2)))

;; Even n with multiple solutions
(check-expect (gb 10) (list (list 5 5) (list 3 7)))

;; Odd n with one solution
(check-expect (gb 9) (list (list 2 7)))

;; n with no solution
(check-expect (gb 11) empty)

;; Small additional examples
(check-expect (gb 6) (list (list 3 3)))
(check-expect (gb 8) (list (list 3 5)))
(check-expect (gb 12) (list (list 5 7)))
(check-expect (gb 14) (list (list 7 7) (list 3 11)))
(check-expect (gb 16) (list (list 5 11) (list 3 13)))
(check-expect (gb 18) (list (list 7 11) (list 5 13)))
(check-expect (gb 20) (list (list 7 13) (list 3 17)))
(check-expect (gb 22) (list(list 11 11) (list 5 17) (list 3 19)))
(check-expect (gb 24) (list (list 11 13)(list 7 17) (list 5 19)))
(check-expect (gb 26) (list (list 13 13) (list 7 19) (list 3 23)))
(check-expect (gb 28) (list (list 11 17) (list 5 23)))
(check-expect (gb 30) (list (list 13 17) (list 11 19) (list 7 23)))

;; big n
(check-expect (gb 100) (list
 (list 47 53)
 (list 41 59)
 (list 29 71)
 (list 17 83)
 (list 11 89)
 (list 3 97)))
(check-expect (gb 128) (list
 (list 61 67)
 (list 31 97)
 (list 19 109)))
(check-expect (gb 109) (list (list 2 107)))



(check-expect (list-primes 10) (list 2 3 5 7))
(check-expect (list-primes 53)
(list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53))
(check-expect (list-primes 2) (list 2))
;; n < 2 → empty list
(check-expect (list-primes 0) '())
(check-expect (list-primes 1) '())

;; small n
(check-expect (list-primes 2) '(2))
(check-expect (list-primes 3) '(2 3))
(check-expect (list-primes 4) '(2 3))
(check-expect (list-primes 5) '(2 3 5))

;; medium n
(check-expect (list-primes 10) '(2 3 5 7))
(check-expect (list-primes 15) '(2 3 5 7 11 13))
(check-expect (list-primes 20) '(2 3 5 7 11 13 17 19))

;; larger n
(check-expect (list-primes 30) '(2 3 5 7 11 13 17 19 23 29))
(check-expect (list-primes 50) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))

(check-expect (prime? 13) true)
(check-expect (prime? 14) false)
(check-expect (prime? 2) true)
(check-expect (prime? 1) false)
(check-expect (prime? 0) false)
;; Numbers less than 2
(check-expect (prime? 0) false)
(check-expect (prime? 1) false)
(check-expect (prime? -5) false)

;; Small primes
(check-expect (prime? 2) true)
(check-expect (prime? 3) true)

;; Small non-primes
(check-expect (prime? 4) false)
(check-expect (prime? 6) false)

;; Medium primes
(check-expect (prime? 13) true)
(check-expect (prime? 17) true)

;; Medium non-primes
(check-expect (prime? 14) false)
(check-expect (prime? 20) false)

;; Large primes
(check-expect (prime? 97) true)

;; Large non-primes
(check-expect (prime? 100) false)

;; Edge cases near divisor boundary (perfect squares)
(check-expect (prime? 9) false)
(check-expect (prime? 25) false)

(check-expect (prime? 11) true)
(check-expect (prime? 13) true)
(check-expect (prime? 17) true)
(check-expect (prime? 19) true)
(check-expect (prime? 23) true)
(check-expect (prime? 29) true)
(check-expect (prime? 31) true)
(check-expect (prime? 37) true)
(check-expect (prime? 41) true)
(check-expect (prime? 43) true)
(check-expect (prime? 47) true)
(check-expect (prime? 53) true)
(check-expect (prime? 59) true)
(check-expect (prime? 61) true)
(check-expect (prime? 67) true)
(check-expect (prime? 71) true)
(check-expect (prime? 73) true)
(check-expect (prime? 79) true)
(check-expect (prime? 83) true)
(check-expect (prime? 89) true)
(check-expect (prime? 97) true)

(check-expect (prime? 10) false)
(check-expect (prime? 12) false)
(check-expect (prime? 14) false)
(check-expect (prime? 15) false)
(check-expect (prime? 16) false)
(check-expect (prime? 18) false)
(check-expect (prime? 20) false)
(check-expect (prime? 21) false)
(check-expect (prime? 22) false)
(check-expect (prime? 24) false)
(check-expect (prime? 25) false)
(check-expect (prime? 26) false)
(check-expect (prime? 27) false)
(check-expect (prime? 28) false)
(check-expect (prime? 30) false)
(check-expect (prime? 32) false)
(check-expect (prime? 33) false)
(check-expect (prime? 34) false)
(check-expect (prime? 35) false)
(check-expect (prime? 36) false)
(check-expect (prime? 38) false)
(check-expect (prime? 39) false)
(check-expect (prime? 40) false)
(check-expect (prime? 42) false)
(check-expect (prime? 44) false)
(check-expect (prime? 45) false)
(check-expect (prime? 46) false)
(check-expect (prime? 48) false)
(check-expect (prime? 49) false)
(check-expect (prime? 50) false)
(check-expect (prime? 51) false)
(check-expect (prime? 52) false)
(check-expect (prime? 54) false)
(check-expect (prime? 55) false)
(check-expect (prime? 56) false)
(check-expect (prime? 57) false)
(check-expect (prime? 58) false)
(check-expect (prime? 60) false)
(check-expect (prime? 62) false)
(check-expect (prime? 63) false)
(check-expect (prime? 64) false)
(check-expect (prime? 65) false)
(check-expect (prime? 66) false)
(check-expect (prime? 68) false)
(check-expect (prime? 69) false)
(check-expect (prime? 70) false)
(check-expect (prime? 72) false)
(check-expect (prime? 74) false)
(check-expect (prime? 75) false)
(check-expect (prime? 76) false)
(check-expect (prime? 77) false)
(check-expect (prime? 78) false)
(check-expect (prime? 80) false)
(check-expect (prime? 81) false)
(check-expect (prime? 82) false)
(check-expect (prime? 84) false)
(check-expect (prime? 85) false)
(check-expect (prime? 86) false)
(check-expect (prime? 87) false)
(check-expect (prime? 88) false)
(check-expect (prime? 90) false)
(check-expect (prime? 91) false)
(check-expect (prime? 92) false)
(check-expect (prime? 93) false)
(check-expect (prime? 94) false)
(check-expect (prime? 95) false)
(check-expect (prime? 96) false)
(check-expect (prime? 98) false)
(check-expect (prime? 99) false)
(check-expect (prime? 100) false)

(check-expect (prime? 1201) true)
(check-expect (prime? 82619) true)

(check-expect (list-primes 100)
              '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
(check-expect (list-primes 101)
              '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101))
