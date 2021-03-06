#lang planet neil/sicp
(define (complete-fermat-test n)
  (complete-fermat-test-helper n (- n 1)))

(define (complete-fermat-test-helper n a)
  (cond ((= a 0) #t)
        ((fermat-test n a) (complete-fermat-test-helper n (- a 1)))
        (else #f)))

(define (fermat-test n a)
  (= (expmod a n n) a))

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
