#lang racket

(require rackcheck/shrink-tree)
(require "proplang.rkt")
(require "shrinking.rkt")

(define (run-generator g sz) (shrink-tree-val (g (current-pseudo-random-generator) sz)))

(define (run-generators generators n)
  (let ([result (make-hash)]
        [size (inexact->exact (round (log n 2)))])
    (for ([p (in-dict-pairs generators)])
      (dict-set! result (car p) (run-generator (cdr p) size)))
    (hash-map/copy result values #:kind 'immutable)))


(struct run-result (search-time shrink-time foundbug passed discards counterexample shrinked-counterexample)
  #:transparent)

(define (run-result->json-str rr)
  (format "[|{\"search-time\": ~a, \"shrink-time\": ~a, \"foundbug\": ~a, \"passed\": ~a, \"discards\": ~a, \"counterexample\": \"~a\", \"shrinked-counterexample\": \"~a\"}|]"
          (run-result-search-time rr)
          (run-result-shrink-time rr)
          (if (run-result-foundbug rr) "true" "false")
          (run-result-passed rr)
          (run-result-discards rr)
          (run-result-counterexample rr)
          (run-result-shrinked-counterexample rr)))

(define (generation-loop tests p generators)
  (let ([start (current-inexact-milliseconds)]
        [foundbug #f]
        [passed 0]
        [discards 0]
        [counterexample #f])
    (let loop ([n 1])
      (if (= n tests)
          'passed
          (let ([env (run-generators generators n)])
            (case (check-property p env)
              [(fail) (set! foundbug #t) (set! counterexample env)]
              [(pass) (set! passed (+ 1 passed)) (loop (+ n 1)) ]
              [(discard) (set! discards (+ 1 discards)) (loop (+ n 1))]))))
    (run-result (- (current-inexact-milliseconds) start) -1 foundbug passed discards counterexample #f)))


(define (run-loop tests p generators)
  (displayln (run-result->json-str (generation-loop tests p generators))))

(define (shrinking-run-loop tests p generators shrinkers)
  (let ([result (generation-loop tests p generators)])
    (if (run-result-foundbug result)
        (begin
          (letrec (
            [start (current-inexact-milliseconds)]
            [shrink-result (shrink-eager p shrinkers (run-result-counterexample result))]
            [search-time (- (current-inexact-milliseconds) start)])
            (displayln (run-result->json-str (run-result (run-result-search-time result)
                                              search-time
                                             (run-result-foundbug result)  
                                             (run-result-passed result)
                                             (run-result-discards result)
                                             (~v (run-result-counterexample result))
                                             (~v shrink-result))))))
        (displayln (run-result->json-str result)))))

(provide run-loop shrinking-run-loop)