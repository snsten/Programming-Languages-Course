
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1.
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; 2.
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


;; 3.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]
        ))

;; 4.
(define (stream-for-n-steps s n)
  (if (< n 1)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; 5.
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 6.
(define dan-then-dog
  (letrec ([f (lambda (x) (if x
                              (cons "dan.jpg" (lambda () (f #f)))
                              (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))

;; 7.
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s)))
                   (stream-add-zero (cdr (s))))))

;; 8.
(define (cycle-lists xs ys)
  (letrec([f (lambda (n) (cons (cons (list-nth-mod xs n)
                                           (list-nth-mod ys n))
                                     (lambda () (f (+ n 1)))))])
    (lambda() (f 0))))

;; 9.
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(> n (- (vector-length vec) 1)) #f]
                      [(and (pair? (vector-ref vec n))
                            (equal? (car (vector-ref vec n)) v)) (vector-ref vec n)]
                      [#t (f (+ n 1))]))])
    (f 0)))

;; 10.
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [idx 0])
    (lambda (v) (if (vector-assoc v cache)
                    (vector-assoc v cache)
                    (let ([curr (assoc v xs)])
                      (begin (vector-set! cache idx  curr)
                             (set! idx
                                   (remainder (+ idx 1) n))
                             (vector-assoc v cache)))))))