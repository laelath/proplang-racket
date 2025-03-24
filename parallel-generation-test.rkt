#lang racket

(require rackcheck/gen/base
         "proplang.rkt"
         "generation.rkt"
         "parallel-generation.rkt")

(define (fib-exp n)
    (cond
      [(= n 0) 0]
      [(= n 1) 1]
      [else (+ (fib-exp (- n 1)) (fib-exp (- n 2)))]))

(define (fib-acc n)
  (let loop ([n n]
             [acc1 0]
             [acc2 1])
    (if (= n 0)
        acc1
        (loop (sub1 n) (+ acc1 acc2) acc1))))

(define fibs-equiv-prop
  (property
   (forall n #:contract exact-integer? #:gen gen:natural)
   (= (fib-exp n) (fib-acc n))))

(run-loop 40 fibs-equiv-prop)
(parallel-run-loop 40 fibs-equiv-prop)