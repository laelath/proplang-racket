#lang racket

(require racket/future
         rackcheck/shrink-tree
         "proplang.rkt" "shrinking.rkt")

(provide parallel-run-loop shrinking-parallel-run-loop)

(define (run-rackcheck-generator g rng sz)
  (shrink-tree-val (g rng sz)))

(struct results (foundbug? passed discards counterexample))

(define (generation-loop tests p [max-workers (processor-count)])

  ;; worker thunk
  (define (worker-future n)
    (define rng (current-pseudo-random-generator))
    (future
     (thunk
      (let-values ([(res env) (generate-and-check p run-rackcheck-generator rng n)])
         (cons res env)))))

  (define num-workers (min tests max-workers))

  (let loop ([workers (build-list num-workers worker-future)]
             [next-test 0]
             [passed 0]
             [discards 0])
    (match workers
      ['() (results #f passed discards #f)]
      [(cons f rst)
       (define res (touch f))
       (case (car res)
         [(fail) (results #t passed discards (cdr res))]
         [(pass discard)
          (loop (if (< (+ next-test num-workers) tests)
                    (append rst (list (worker-future (+ next-test num-workers))))
                    rst)
                (add1 next-test)
                (if (eq? (car res) 'pass) (add1 passed) passed)
                (if (eq? (car res) 'discards) (add1 discards) discards))])])))

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

(define (parallel-run-loop tests p [workers (processor-count)])
  (define-values (res real) (real-time generation-loop tests p workers))
  (displayln (results->json-str real res)))

(define (shrinking-parallel-run-loop tests p)
  (define-values (res search-time) (real-time generation-loop tests p))
  (if (results-foundbug? res)
      (let-values ([(shrunk shrink-time) (real-time shrink-eager p (results-counterexample res))])
        (displayln (shrink-results->json-str search-time shrink-time shrunk res)))
      (displayln (shrink-results->json-str search-time #f #f res))))