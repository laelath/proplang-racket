#lang racket

(require racket/future
         rackcheck/shrink-tree
         "proplang.rkt" "shrinking.rkt")

(provide parallel-run-loop shrinking-parallel-run-loop)

(define (run-rackcheck-generator g rng sz)
  (shrink-tree-val (g rng sz)))

(struct results (foundbug? passed discards counterexample))

(define (box-faa! b n)
  (define m (unbox b))
  (if (box-cas! b m (+ m n))
      m
      (box-faa! b n)))

(define (generation-loop tests p [num-workers (processor-count)])

  (define counter (box 0))
  (define found-counterexample? (box #f))

  (define (worker-thunk)
    ;; each thread has its own random number generator
    ;; caution - might be seeding with the same value (uses current-milliseconds to seed)
    (define rng (make-pseudo-random-generator))
    (let worker-loop ([passed 0]
                      [discards 0])
      (define n (box-faa! counter 1))
      (cond
        [(>= n tests) (results #f passed discards #f)]
        [(unbox found-counterexample?) (results #f passed discards #f)]
        [else
         (let-values ([(res env) (generate-and-check p run-rackcheck-generator rng (inexact->exact (round (log (+ n 1) 2))))])
           (case res
             [(fail)
              (set-box! found-counterexample? #t)
              (results #t passed discards env)]
             [(pass) (worker-loop (add1 passed) discards)]
             [(discard) (worker-loop passed (add1 discards))]))])))

  ;; spawn workers
  (define workers (build-list num-workers (λ (_) (future worker-thunk))))

  ;; touch workers and merge results
  (for/fold ([res (results #f 0 0 #f)])
            ([worker workers])
    (define worker-res (touch worker))
    (results (or (results-foundbug? res) (results-foundbug? worker-res))
             (+ (results-passed res) (results-passed worker-res))
             (+ (results-discards res) (results-discards worker-res))
             (or (results-counterexample res) (results-counterexample worker-res)))))


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