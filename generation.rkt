#lang racket

(require rackcheck/shrink-tree)
(require "proplang.rkt" "shrinking.rkt")

(provide run-loop shrinking-run-loop)

(define (run-rackcheck-generator g sz) (shrink-tree-val (g (current-pseudo-random-generator) sz)))

(define (run-generators generators n)
  (let ([size (inexact->exact (round (log n 2)))])
    (dict-map/copy generators
                   (lambda (key gen) (values key (run-rackcheck-generator gen size))))))

(struct run-result (search-time foundbug passed discards counterexample)
  #:transparent)

(struct shrink-result (shrink-time shrunk-counterexample)
  #:transparent)

(define (run-result->json-str rr sr)
  (format "[|{\"search-time\": ~a, \"shrink-time\": ~a, \"foundbug\": ~a, \"passed\": ~a, \"discards\": ~a, \"counterexample\": \"~a\", \"shrunk-counterexample\": \"~a\"}|]"
          (run-result-search-time rr)
          (if sr (shrink-result-shrink-time sr) "n/a")
          (if (run-result-foundbug rr) "true" "false")
          (run-result-passed rr)
          (run-result-discards rr)
          (run-result-counterexample rr)
          (if sr (shrink-result-shrunk-counterexample sr) "n/a")))

(define (generation-loop tests p generators)
  (define start (current-inexact-monotonic-milliseconds))
  (let loop ([n 0]
             [passed 0]
             [discards 0])
    (if (= n tests)
        (run-result (- (current-inexact-monotonic-milliseconds) start) #f passed discards #f)
        (let ([env (run-generators generators n)])
          (case (check-property p env)
            [(fail) (run-result (- (current-inexact-monotonic-milliseconds) start)
                                #t passed discards env)]
            [(pass) (loop (add1 n) (add1 passed) discards)]
            [(discard) (loop (add1 n) passed (add1 discards))])))))

(define (run-loop tests p generators)
  (displayln (run-result->json-str (generation-loop tests p generators) #f)))

(define (shrinking-run-loop tests p generators shrinkers)
  (define result (generation-loop tests p generators))
  (if (run-result-foundbug result)
      (let* ([start (current-inexact-monotonic-milliseconds)]
             [shrink-result (shrink-eager p shrinkers (run-result-counterexample result))]
             [search-time (- (current-inexact-monotonic-milliseconds) start)])
        (displayln (run-result->json-str
                    result
                    (shrink-result search-time shrink-result))))
      (displayln (run-result->json-str result))))

