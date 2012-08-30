#lang racket

;; square a value
(define (sq x) (* x x))

;; find the absolute value for a number using cond
;; section 1.1.6 
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

;; absolute using if
(define (abs-better x)
  (if (< x 0)
      (- x)
      x))

;; square roots by newtons methods
(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
         
(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;; fine the same function, but use local funcs
(define (sqrt-closure x)
  (define (good-enough? guess)
    (< (abs (- square guess)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess 
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; end of square roots


;; linear recursion version of factorial 
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; linear process for factorials 

(define (factorial-linear n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))


;; tree recursion 
;; section 1.2.2 
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


;; procedures as arguments
;; section 1.3.1

(define (inc x) (+ x 1)) ;; this seems to be missing from racket
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (id x) x)

(define (sum-integers a b)
  (sum id a inc b))





