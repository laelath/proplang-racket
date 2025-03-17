#lang racket

(require racket/dict)

(require "proplang.rkt")

(provide
 (contract-out
  [shrink-eager (property? dict? . -> . dict?)]
  [shrink-eager-retry (property? dict? . -> . dict?)]))

(define (shrink-eager prop env)
  (define (shrink-var prop env x shrinker)
    (let down ([v (dict-ref env x)])
      (let across ([shrinks (shrinker v)])
        (match shrinks
          ['() v]
          [(cons v vs)
           (if (equal? (check-property prop (dict-set env x v)) 'fail)
               (down v)
               (across vs))]))))
  (match prop
    [(Forall x augments body)
     (define contract ((dict-ref augments '#:contract (const any/c)) env))
     (if (dict-has-key? augments '#:shrink)
         (let* ([shrinker (invariant-assertion
                           (-> contract (listof contract))
                           ((dict-ref augments '#:shrink) env))]
                [v (shrink-var body env x shrinker)])
           (shrink-eager body (dict-set env x v)))
         (shrink-eager body env))]
    [(Implies _ body) (shrink-eager body env)]
    [(Check _) env]))

(define (dict-equal? d1 d2 [proc equal?])
  (for/and ([(k v) (in-dict d1)])
    (and (dict-has-key? d2 k) (proc v (dict-ref d2 k)))))

(define (shrink-eager-retry prop env)
  (define new-env (shrink-eager prop env))
  (if (dict-equal? env new-env eq?)
      env
      (shrink-eager-retry prop new-env)))