#lang racket

(require racket/stxparam)
(require (for-syntax syntax/parse syntax/transformer))

(provide
 property
 property?
 add-augments
 (contract-out
  [check-property (-> property? dict? (or/c 'discard 'fail 'pass))]
  ;; TODO: contracts for generation functions
  [generate (-> property? any/c any/c ... dict?)]
  [generate-and-check (-> property? any/c any/c ... (values (or/c 'pass 'fail 'discard) dict?))]
  [struct Forall ([var symbol?] [augments dict?] [body property?])]
  [struct Implies ([prop (-> dict? any/c)] [body property?])]
  [struct Check ([prop (-> dict? any/c)])]))

(struct Forall (var augments body) #:transparent)
(struct Implies (prop body) #:transparent)
(struct Check (prop) #:transparent)

(define (property? p)
  (or (Forall? p) (Implies? p) (Check? p)))

(define-syntax-parameter property-env #f)

(define-syntax (property stx)
  (syntax-parse stx
    #:datum-literals (forall implies)
    [(_ (forall x:id (~seq kw:keyword e:expr) ...) ~rest rst)
     #'(let-syntax ([x (make-variable-like-transformer
                        (λ (stx)
                          #`(dict-ref #,(syntax-parameter-value #'property-env) 'x)))])
         (Forall 'x (hash (~@ 'kw (λ (env)
                                    (syntax-parameterize
                                        ([property-env #'env])
                                      e)))
                          ...)
                 (property . rst)))]
    [(_ (implies e) ~rest rst)
     #'(Implies (λ (env)
                  (syntax-parameterize
                      ([property-env #'env])
                    e))
                (property . rst))]
    [(_ e) #'(Check (λ (env)
                      (syntax-parameterize
                          ([property-env #'env])
                        e)))]))

(define add-augments
  (make-keyword-procedure
   (lambda (kws kw-args p x)
     (let loop ([p p])
       (match p
         [(Forall y augments body)
          (if (eq? x y)
              (Forall y (foldl (λ (kw arg augs)
                                 (dict-set augs kw arg))
                               augments
                               kws kw-args)
                      body)
              (Forall y augments (loop body)))]
         [(Implies prop body) (Implies prop (loop body))]
         [(Check prop) (Check prop)])))))

(define (check-property p env)
  (match p
    [(Forall var augments body)
     (unless (dict-has-key? env var)
       (error 'missing-variable))
     (when (dict-has-key? augments '#:contract)
       (invariant-assertion ((dict-ref augments '#:contract) env) (dict-ref env var)))
     (check-property body env)]
    [(Implies prop body)
     (if (prop env) (check-property body env) 'discard)]
    [(Check prop)
     (if (prop env) 'pass 'fail)]))

(define (generate p sample . args)
  (let loop ([p p]
             [env (hash)])
    (match p
      [(Forall var augments body)
       (unless (dict-has-key? augments '#:gen)
         (error 'no-generator))
       (define val (apply sample ((dict-ref augments '#:gen) env) args))
       (when (dict-has-key? augments '#:contract)
         (invariant-assertion ((dict-ref augments '#:contract) env) val))
       (loop body (dict-set env var val))]
      [(Implies prop body)
       (loop body env)]
      [(Check prop) env])))

(define (generate-and-check p sample . args)
  (let loop ([p p]
             [env (hash)])
    (match p
      [(Forall var augments body)
       (unless (dict-has-key? augments '#:gen)
         (error 'no-generator))
       (define val (apply sample ((dict-ref augments '#:gen) env) args))
       (when (dict-has-key? augments '#:contract)
         (invariant-assertion ((dict-ref augments '#:contract) env) val))
       (loop body (dict-set env var val))]
      [(Implies prop body)
       (if (prop env)
           (loop body env)
           (values 'discard env))]
      [(Check prop)
       (if (prop env)
           (values 'pass env)
           (values 'fail env))])))