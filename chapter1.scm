#lang racket

(define (square x)
  (* x x))

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

;; section 1.2.4
;; linear process requires 0(n) steps and 0(n) space
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; 0(n) steps, 0(1) space
(define (expt2 b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

;; fast expt


;; procedures as arguments
;; section 1.3.1

(define (inc x) (+ x 1))
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

;; section 1.3.2
;; using lambda to construct functions
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a (lambda (x) (+ x 4)) b))

;; using let to create local variables

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; section 1.3.3 


(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))

(define (search f neg-point po-point)
  (let ((midpoint (average neg-point po-point)))
    (if (close-enough? neg-point po-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint po-point))
                (else midpoint))))))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

;; approximate to pie
(half-interval-method sin 2.0 4.0)





