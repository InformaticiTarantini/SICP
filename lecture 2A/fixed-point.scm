#lang planet neil/sicp
(define tolerance 0.00001)

(define (sqrt x)
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
    (lambda (x) (average x (f x))))

  (fixed-point (average-damp
                (lambda (y) (/ x y)))
               1.0))