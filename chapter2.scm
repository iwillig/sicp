#lang racket

(define (square x) (* x x))


;; chapter 2, Building abstraction with data

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


;; section 2.2.1 sequences

(define lst1 (cons 1 (cons 2 (cons 3 (cons 4 null)))))
(define lst2 (list 1 2 3 4))

;; (car lst1)
;; (cdr lst2)

;; find the length of a list by a recursive plan
;; 0(n) time, 0(n) space
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (mymap func items)
  (if (null? items)
      null
      (cons (func (car items))
            (mymap func (cdr items)))))


;; section 2.2.2 hierachial structures

(define x (cons (list 1 2) (list 3 4)))


(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(define (fringe x) '())


;; mapping over trees 
(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;;(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)


;; section 2.2.3

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))





(define (myfilter predicate seq)
  (cond ((null? seq) null)
        ((predicate (car seq))
         (cons (car seq)
               (myfilter predicate (cdr seq))))
        (else (myfilter predicate (cdr seq)))))

;; (myfilter odd? (list 1 2 3 4 5))

(define (accu op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accu op initial (cdr seq)))))

;; (accu + 0 (list 1 2 3 4 5))


