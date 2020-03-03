#lang racket

;; 1.

(define (string-ith str i)
  (substring str i (add1 i)))

(define str "helloworld")
(define i 5)

;; (println (string-append (substring str 0 i) "_" (substring str i)))
;; (println (string-append (substring str 0 i) (substring str (add1 i))))

(define (string-empty? str)
  (zero? (string-length str)))

(define (apply-for-empty-string f)
  (λ (str) (if (string-empty? str) "" (f str))))

(define string-first (apply-for-empty-string
                      (λ (str) (substring str 0 1))))

(define string-last (apply-for-empty-string
                     (λ (str)
                       (define len (string-length str))
                       (substring str (sub1 len) len))))


(define (divisors n)
  (filter (λ (m) (zero? (remainder n m)))
          (range 1 (add1 n))))