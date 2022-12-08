#lang racket

;; script for filtering out the benchmarks in Herbie that
;; only contain rational operators.

(define (ops-in-expr expr)
  (define ops (mutable-set))
  (let loop ([expr expr])
    (match expr
     [(list 'let (list (list vars vals) ...) body)
      (set-add! ops 'let)
      (for ([val vals]) (loop val))
      (loop body)]
     [(list 'let* (list (list vars vals) ...) body)
      (set-add! ops 'let*)
      (for ([val vals]) (loop val))
      (loop body)]
     [(list op args ...)
      (set-add! ops op)
      (for ([arg args]) (loop arg))]
     [_ (void)]))
  (map symbol->string (set->list ops)))

(define (syntax-e* stx)
  (match (syntax-e stx)
   [(list xs ...) (map syntax-e* xs)]
   [x x]))

(define (make-pairs l acc)
  (if (= 0 (modulo (length l) 2))
      (cond [(empty? l) acc]
            [else (make-pairs (rest (rest l)) (cons (cons (first l) (first (rest l))) acc))])
      (error "The list is not pair-able (uneven length) ~a" l)))

(define (load-file file ops names one-of?)
  (define (valid? in-expr)
    (if one-of?
        (ormap (curry set-member? ops) in-expr)
        (subset? in-expr ops)))
  (call-with-input-file file
    (λ (port)
      (port-count-lines! port)
      (for ([core (in-port (curry read-syntax file) port)])
        (define core* (syntax-e* core))
        (define-values (vars props body)
          (match core*
            [(list 'FPCore (list vars ...) props ... body)
             (values vars props body)]
            [(list 'FPCore name (list vars ...) props ... body)
             (values vars props body)]))
        (define in-expr (ops-in-expr body))
        (define ps (make-hash (make-pairs props '())))
        (define nm (string-trim (hash-ref ps ':name "")))
        (when (and (valid? in-expr) (not (member nm names)))
          (pretty-print core* (current-output-port) 1)
          (newline))))))

(define (load-directory dir ops names one-of?)
  (for ([fname (in-directory dir)]
        #:when (file-exists? fname)
        #:when (equal? (filename-extension fname) #"fpcore"))
    (load-file fname ops names one-of?)))

(define (filter-cores path ops names one-of?)
  (define path* (if (string? path) (string->path path) path))
  (define out
    (cond
     [(directory-exists? path*)
      (load-directory path* ops names one-of?)]
     [else
      (load-file path* ops names one-of?)]))
  (void))
  
(module+ main
 (define one-of? #f)
 (define ops '())
 (define names '())
 (command-line
  #:program "filter.rkt"
  #:once-each
  [("--or") "Make the filter define `at-least-one`"
   (set! one-of? #t)]
  ["--operators" str "List of operators (single string)"
   (set! ops (string-split str " "))]
  ["--names" strs "Names of bad fp-cores (; separated single string)"
    (set! names (map (lambda (x) (string-trim x)) (string-split strs ";")))]
  #:args paths
 (for ([path paths]) (filter-cores path ops names one-of?))
 (void)))
