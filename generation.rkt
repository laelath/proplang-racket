#lang racket

(require rackcheck/shrink-tree)
(require "proplang.rkt" "shrinking.rkt")

(provide run-loop shrinking-run-loop)

(define (run-rackcheck-generator g sz)
  (shrink-tree-val (g (current-pseudo-random-generator) sz)))

(struct results (foundbug? passed discards counterexample))

(define (generation-loop tests p)
  (let loop ([n 0]
             [passed 0]
             [discards 0])
    (if (= n tests)
        (results #f passed discards #f)
        (let-values ([(res env) (generate-and-check p run-rackcheck-generator n)])
          (case res
            [(fail) (results #t passed discards env)]
            [(pass) (loop (add1 n) (add1 passed) discards)]
            [(discard) (loop (add1 n) passed (add1 discards))])))))

(define (results->json-str search-time res)
  (format "[|{\"search-time\": ~a, \"foundbug\": ~a, \"passed\": ~a, \"discards\": ~a, \"counterexample\": \"~a\"}|]"
          search-time
          (if (results-foundbug? res) "true" "false")
          (results-passed res)
          (results-discards res)
          (results-counterexample res)))

(define (shrink-results->json-str search-time shrink-time shrunk res)
  (format "[|{\"search-time\": ~a, \"shrink-time\": ~a, \"foundbug\": ~a, \"passed\": ~a, \"discards\": ~a, \"counterexample\": \"~a\", \"shrunk-counterexample\": \"~a\"}|]"
          search-time
          shrink-time
          (if (results-foundbug? res) "true" "false")
          (results-passed res)
          (results-discards res)
          (results-counterexample res)
          shrunk))

(define (real-time proc . lst)
  (define-values (res _cpu real _gc) (time-apply proc lst))
  (values (first res) real))

(define (run-loop tests p)
  (define-values (res real) (real-time generation-loop tests p))
  (displayln (results->json-str real res)))

(define (shrinking-run-loop tests p)
  (define-values (res search-time) (real-time generation-loop tests p))
  (if (results-foundbug? res)
      (let-values ([(shrunk shrink-time) (real-time shrink-eager p (results-counterexample res))])
        (displayln (shrink-results->json-str search-time shrink-time shrunk res)))
      (displayln (shrink-results->json-str search-time #f #f res))))