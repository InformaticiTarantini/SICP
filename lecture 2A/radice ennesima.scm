#lang planet neil/sicp

(define (pot x n)
  (pot-iter x n 1))

(define (pot-iter x counter product)
  (if (= counter 0)
      product
      (pot-iter x
                (- counter 1)
                (* x product))))

(define tolerance 0.00001)

(define (nth-root x n)
  (define (fixed-point f start)
    (define (close-enough? old new)
      (< (abs (- old new)) tolerance))
    (define (iter old new)
      (if (close-enough? old new)
          new
          (iter new (f new))))
    (iter start (f start)))
  
  (define (average x y)
    (/ (+ x y) 2))
  
  (define (average-damp f)
    (lambda (z) (average z (f z))))
  
  (fixed-point (average-damp
                (lambda (y) (/ x (pot y (- n 1)))))
               2.0))