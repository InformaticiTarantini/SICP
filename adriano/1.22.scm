#lang planet neil/sicp

(define (square n) (* n n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (check-nontrivial-sqrt n m)
  (let ((x (remainder (square n) m)))
    (if (and (not (= n 1)) (not (= n (- m 1))) (= x 1))
        0
        x)))

(define (miller-rabin-test n a)
  (= (expmod a (- n 1) n) 1))

(define (prime? n)
  (cond ((= n 2) #t)
        ((even? n) #f)
        (else
         (prime-helper n (- n 1)))))

(define (prime-helper n a)
  (cond ((= a 0) #t)
        ((miller-rabin-test n a) (miller-rabin-test n (- a 1)))
        (else #f)))



(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
    (cond ((<= a b) (timed-prime-test a) (search-for-primes (+ a 1) b))))

