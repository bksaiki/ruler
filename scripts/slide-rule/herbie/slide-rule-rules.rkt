#lang racket

;; Arithmetic identities for rewriting programs.

(require "../common.rkt" "../errors.rkt" "types.rkt" "syntax.rkt" "sugar.rkt")

(provide (struct-out rule) *rules* *simplify-rules* *fp-safe-simplify-rules*)
(module+ internals (provide define-ruleset define-ruleset* register-ruleset!
                            *rulesets* generate-rules-for *templated-reprs*))

;; Rulesets
(define *rulesets* (make-parameter '()))

;; Cached rules
(define all-rules (make-parameter '()))
(define simplify-rules (make-parameter '()))
(define fp-safe-simplify-rules (make-parameter '()))

;; Exported parameters to update and access rules
(define *rules*
  (make-derived-parameter all-rules 
                          identity 
                          (λ (_) (generate-missing-rules) (all-rules))))

(define *simplify-rules*
  (make-derived-parameter simplify-rules 
                          identity 
                          (λ (_) (generate-missing-rules) (simplify-rules))))

(define *fp-safe-simplify-rules*
  (make-derived-parameter fp-safe-simplify-rules 
                          identity 
                          (λ (_) (generate-missing-rules) (fp-safe-simplify-rules))))

;; Update parameters

;; Note on rules
;; fp-safe-simplify ⊂ simplify ⊂ all
;;
;; all                    requires at least one tag of an active group of rules
;; simplify               same req. as all + 'simplify' tag
;; fp-safe-simplify       same req. as simplify + 'fp-safe' tag       ('fp-safe' does not imply 'simplify')
;;

(struct rule (name input output itypes otype)
        ;; Input and output are patterns
        ;; itypes is a mapping, variable name -> representation
        ;; otype is a representation
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (fprintf port "#<rule ~a>" (rule-name rule)))])

(define (rule-ops-supported? rule)
  (define (ops-in-expr expr)
    (cond
      [(list? expr)
       (and (impl-exists? (car expr))
            (for/and ([subexpr (cdr expr)])
              (ops-in-expr subexpr)))]
      [else true]))
  (ops-in-expr (rule-output rule)))

(register-reset
 #:priority 10 ; Must be higher than priority for pruning operators
 (λ ()
   (*rulesets*
    (for/list ([ruleset (*rulesets*)])
      (match-define (list rules groups types) ruleset)
      (list (filter rule-ops-supported? rules) groups types)))))

(define (update-rules rules groups)
  (when (ormap (curry flag-set? 'rules) groups)       ; update all
    (all-rules (append (all-rules) rules))
    (when (set-member? groups 'simplify)              ; update simplify
      (simplify-rules (append (simplify-rules) rules))  
      (when (set-member? groups 'fp-safe)         ; update fp-safe
        (fp-safe-simplify-rules (append (fp-safe-simplify-rules) rules))))))

(define (reprs-in-expr expr)
  (remove-duplicates
    (let loop ([expr expr])
      (match expr
      [(list 'if cond ift iff)
        (append (loop cond) (loop ift) (loop iff))]
      [(list op args ...)
        (append (operator-info op 'itype) (append-map loop args))]
      [_ '()]))))

(define (type-of-rule input output ctx)
  (cond   ; special case for 'if', return the 'type-of-rule' of the ift branch
    [(list? input) 
      (if (equal? (car input) 'if)
          (type-of-rule (caddr input) output ctx)
          (operator-info (car input) 'otype))]
    [(list? output)
      (if (equal? (car output) 'if)
          (type-of-rule input (caddr output) ctx)
          (operator-info (car output) 'otype))]
    [(symbol? input) (dict-ref ctx input)]   ; fallback: if symbol, check ctx for type
    [(symbol? output) (dict-ref ctx output)]
    [else
      (error 'type-of-rule "Could not compute type of rule ~a -> ~a"
              input output)]))

;; Rulesets defined by reprs. These rulesets are unique
(define (register-ruleset! name groups var-ctx rules)
  (define rules*
    (for/list ([r rules])
      (match-define (list rname input output) r)
      (rule rname input output var-ctx
            (type-of-rule input output var-ctx))))
  (*rulesets* (cons (list rules* groups var-ctx) (*rulesets*))))
      
(define-syntax define-ruleset
  (syntax-rules ()
   [(define-ruleset name groups [rname input output] ...)
    (define-ruleset name groups #:type () [rname input output] ...)]
   [(define-ruleset name groups #:type ([var type] ...) [rname input output] ...)
    (register-ruleset! 'name 'groups `((var . ,(get-representation 'type)) ...)
                       '((rname input output) ...))]))

;; Templated rulesets defined by types. These are used to generate duplicate rules that
;; are valid in any representation of the same underlying type.

(define *templated-rulesets* (make-parameter '()))
(define *templated-reprs* (make-parameter '()))

(define-syntax define-ruleset*
  (syntax-rules ()
    [(_ name groups [rname input output] ...)
     (define-ruleset* name groups #:type () [rname input output] ...)]
    [(_ name groups #:type ([var type] ...) [rname input output] ...)
     (begin
      (define name (list (rule 'rname 'input 'output '((var . type) ...) 'unknown) ...))
      (*templated-rulesets* (cons (list name 'groups '((var . type) ...)) 
                                  (*templated-rulesets*))))]))

(define *reprs-with-rules* (make-parameter '()))

;; Add existing rules in rulesets to 'active' rules

(define (add-rules-from-rulesets repr)
  (*reprs-with-rules* (set-add (*reprs-with-rules*) repr)) ; update
  (define valid? (curry set-member? (*reprs-with-rules*)))

  (define (valid-rule r)
    (define in-reprs (reprs-in-expr (rule-input r)))
    (define out-reprs (reprs-in-expr (rule-output r)))
    (define all-reprs (set-union (list (rule-otype r)) in-reprs out-reprs))
    (and (andmap valid? all-reprs) (ormap (curry equal? repr) all-reprs)))

  (for ([set (*rulesets*)])
    (match-define `((,rules ...) (,groups ...) ((,vars . ,types) ...)) set)
    (when (andmap valid? types)
      (define rules* (filter valid-rule rules))
      (unless (empty? rules*)   ; only add the ruleset if it contains one
        (update-rules rules* groups)))))

;; Generate a set of rules by replacing a generic type with a repr
(define (generate-rules-for type repr)
  (define valid? (disjoin (curry equal? type)
                          (curry set-member? (map representation-name (*reprs-with-rules*)))))
  (*templated-reprs* (set-add (*templated-reprs*) repr)) ; update
  (for ([set (reverse (*templated-rulesets*))] ; preserve rule order
       #:when (or (empty? (third set)) ; no type ctx
                  (andmap (λ (p) (valid? (cdr p))) (third set))))
    (match-define `((,rules ...) (,groups ...) ((,vars . ,types) ...)) set)
    (define var-reprs
      (for/list ([v vars] [t types])
        (if (equal? t type) repr (get-representation t))))
    (define ctx (context vars repr var-reprs))
    (define vrhash (map cons vars var-reprs))
    (define rules*
      (for/fold ([rules* '()]) ([r rules])
        (with-handlers ([exn:fail:user:herbie:missing? (const rules*)])
          (define name* (sym-append (rule-name r) '_ (representation-name repr)))
          (define input* (desugar-program (rule-input r) ctx #:full #f))
          (define output* (desugar-program (rule-output r) ctx #:full #f))
          (define rule* (rule name* input* output* vrhash (type-of-rule input* output* vrhash)))
          (cons rule* rules*))))
    (unless (empty? rules*)   ; only add the ruleset if it contains one
      (*rulesets* (cons (list rules* groups vrhash) (*rulesets*))))))

;; Generate rules for new reprs

(define (generate-missing-rules)
  (for ([repr (*needed-reprs*)])
    (unless (set-member? (*templated-reprs*) repr)
      (generate-rules-for (representation-type repr) repr))
    (unless (set-member? (*reprs-with-rules*) repr)
      (add-rules-from-rulesets repr))))

(define-ruleset* compare-reduce (bools simplify fp-safe-nan)
  #:type ([x real] [y real])
  [lt-same      (<  x x)         (FALSE)]
  [gt-same      (>  x x)         (FALSE)]
  [lte-same     (<= x x)         (TRUE)]
  [gte-same     (>= x x)         (TRUE)]
  [not-lt       (not (<  x y))   (>= x y)]
  [not-gt       (not (>  x y))   (<= x y)]
  [not-lte      (not (<= x y))   (>  x y)]
  [not-gte      (not (>= x y))   (<  x y)])

(define-ruleset* branch-reduce (branches simplify fp-safe)
  #:type ([a bool] [b bool] [x real] [y real])
  [if-true        (if (TRUE) x y)       x]
  [if-false       (if (FALSE) x y)      y]
  [if-same        (if a x x)          x]
  [if-not         (if (not a) x y)    (if a y x)]
  [if-if-or       (if a x (if b x y)) (if (or a b) x y)]
  [if-if-or-not   (if a x (if b y x)) (if (or a (not b)) x y)]
  [if-if-and      (if a (if b x y) y) (if (and a b) x y)]
  [if-if-and-not  (if a (if b y x) y) (if (and a (not b)) x y)])

;; Ruler rules
(define-ruleset*
 ruler-simplify
 (ruler simplify arithmetic)
 #:type
 ((a real) (b real) (c real))
 (rules-1 (* c (* b a)) (* b (* a c)))
 (rules-2 (* b (* a c)) (* c (* b a)))
 (rules-3 (+ c (+ b a)) (+ a (+ b c)))
 (rules-4 (/ (/ c b) a) (/ (/ c a) b))
 (rules-5 (- (- c b) a) (- (- c a) b))
 (rules-6 (/ c (/ b a)) (/ a (/ b c)))
 (rules-7 (/ c (* b a)) (/ (/ c a) b))
 (rules-8 (/ (/ c a) b) (/ c (* b a)))
 (rules-9 (- c (+ b a)) (- (- c a) b))
 (rules-10 (- (- c a) b) (- c (+ b a)))
 (rules-11 (/ (* c b) a) (* c (/ b a)))
 (rules-12 (* c (/ b a)) (/ (* c b) a))
 (rules-13 (/ c (/ b a)) (* c (/ a b)))
 (rules-14 (* c (/ a b)) (/ c (/ b a)))
 (rules-15 (/ c (- b (- b a))) (/ c a))
 (rules-16 (/ (- c (- c b)) a) (/ b a))
 (rules-17 (+ (/ c a) (/ b a)) (/ (+ c b) a))
 (rules-18 (/ (+ c b) a) (+ (/ c a) (/ b a)))
 (rules-19 (* a (- c b)) (- (* a c) (* b a)))
 (rules-20 (- (* a c) (* b a)) (* a (- c b)))
 (rules-21 (- (/ c a) (/ b a)) (/ (- c b) a))
 (rules-22 (/ (- c b) a) (- (/ c a) (/ b a)))
 (rules-23 (* b (+ c a)) (+ (* c b) (* b a)))
 (rules-24 (+ (* c b) (* b a)) (* b (+ c a)))
 (rules-25 (/ (+ c (- a b)) a) (/ (+ a (- c b)) a))
 (rules-26 (/ (+ a (- c b)) a) (/ (+ c (- a b)) a))
 (rules-27 (/ (+ c (- 2 b)) a) (/ (+ 2 (- c b)) a))
 (rules-28 (/ (+ 2 (- c b)) a) (/ (+ c (- 2 b)) a))
 (rules-29 (+ b a) (+ a b))
 (rules-30 (* b a) (* a b))
 (rules-31 (- a b) (neg (- b a)))
 (rules-32 (neg (- b a)) (- a b))
 (rules-33 (+ b (neg a)) (- b a))
 (rules-34 (- b a) (+ b (neg a)))
 (rules-35 (* b (neg a)) (neg (* b a)))
 (rules-36 (neg (* b a)) (* b (neg a)))
 (rules-37 (/ (neg b) a) (/ b (neg a)))
 (rules-38 (/ b (neg a)) (/ (neg b) a))
 (rules-39 (/ b (neg a)) (neg (/ b a)))
 (rules-40 (neg (/ b a)) (/ b (neg a)))
 (rules-41 (* a (/ b a)) b)
 (rules-42 (* (- b a) (+ a b)) (- (* b b) (* a a)))
 (rules-43 (- (* b b) (* a a)) (* (- b a) (+ a b)))
 (rules-44 (/ (- a b) a) (- 1 (/ b a)))
 (rules-45 (- 1 (/ b a)) (/ (- a b) a))
 (rules-46 (/ (+ a b) a) (+ 1 (/ b a)))
 (rules-47 (+ 1 (/ b a)) (/ (+ a b) a))
 (rules-48 (- b (* b a)) (* b (- 1 a)))
 (rules-49 (* b (- 1 a)) (- b (* b a)))
 (rules-50 (+ a (* b a)) (* a (+ b 1)))
 (rules-51 (* a (+ b 1)) (+ a (* b a)))
 (rules-52 (/ b (+ b (/ b a))) (/ a (+ a 1)))
 (rules-53 (/ a (- (/ a b) a)) (/ b (- 1 b)))
 (rules-54 (/ a (- (* a b) a)) (/ 1 (+ b -1)))
 (rules-56 (neg (neg a)) a)
 (rules-58 (- a 0) a)
 (rules-60 (* a 1) a)
 (rules-62 (/ a 1) a)
 (rules-64 (+ a 0) a)
 (rules-65 (- a a) 0)
 (rules-66 (/ a a) 1)
 (rules-67 (neg a) (- 0 a))
 (rules-68 (- 0 a) (neg a))
 (rules-69 (neg a) (/ a -1))
 (rules-70 (/ a -1) (neg a))
 (rules-71 (neg a) (* a -1))
 (rules-72 (* a -1) (neg a))
 (rules-73 (+ a a) (* a 2))
 (rules-74 (* a 2) (+ a a))
 (rules-75 (/ 0 a) 0)
 (rules-76 (* a 0) 0)
 (rules-77 (- a 1) (+ a -1))
 (rules-78 (+ a -1) (- a 1))
 (rules-79 (- a -1) (+ a 1))
 (rules-80 (+ a 1) (- a -1))
 (rules-81
  (- (* (sin b) (sin b)) (* (cos a) (cos a)))
  (- (* (sin a) (sin a)) (* (cos b) (cos b))))
 (rules-82
  (- (* (sin b) (sin b)) (* (sin a) (sin a)))
  (- (* (cos a) (cos a)) (* (cos b) (cos b))))
 (rules-83
  (- (* (cos a) (cos a)) (* (cos b) (cos b)))
  (- (* (sin b) (sin b)) (* (sin a) (sin a))))
 (rules-84 (cos (neg a)) (cos a))
 (rules-85 (cos a) (cos (neg a)))
 (rules-86 (tan (neg a)) (neg (tan a)))
 (rules-87 (neg (tan a)) (tan (neg a)))
 (rules-88 (sin (neg a)) (neg (sin a)))
 (rules-89 (neg (sin a)) (sin (neg a)))
 (rules-90 (tan (+ a (PI))) (tan a))
 (rules-91 (tan a) (tan (+ a (PI))))
 (rules-92 (sin (- (PI) a)) (sin a))
 (rules-93 (sin a) (sin (- (PI) a)))
 (rules-94 (cos (+ (PI) a)) (neg (cos a)))
 (rules-95 (neg (cos a)) (cos (+ (PI) a)))
 (rules-96 (+ (* (cos a) (cos a)) (* (sin a) (sin a))) 1)
 (rules-97 (tan (PI)) (sin (PI)))
 (rules-98 (sin (PI)) (tan (PI)))
 (rules-99 (sin (PI)) (sin (+ (PI) (PI))))
 (rules-100 (sin (+ (PI) (PI))) (sin (PI)))
 (rules-101 (tan (+ (PI) (PI))) (sin (PI)))
 (rules-102 (sin (PI)) (tan (+ (PI) (PI))))
 (rules-104 (cos (PI)) -1)
 (rules-106 (sin (PI)) 0)
 (rules-108 (cos (+ (PI) (PI))) 1)
 (rules-110 (cos 0) 1)
 (rules-112 (tan 0) 0)
 (rules-114 (sin 0) 0)
 (rules-116 (sin (/ (PI) 2)) 1)
 (rules-118 (cos (/ (PI) 2)) 0)
 (rules-119 (pow (pow c b) a) (pow (pow c a) b))
 (rules-120 (pow c (* b a)) (pow (pow c a) b))
 (rules-121 (pow (pow c a) b) (pow c (* b a)))
 (rules-122 (pow (exp c) (* b a)) (pow (exp a) (* b c)))
 (rules-123 (* c (log (pow b a))) (* a (log (pow b c))))
 (rules-124 (pow c (* b (log a))) (pow a (* b (log c))))
 (rules-125 (pow (* c b) a) (* (pow c a) (pow b a)))
 (rules-126 (* (pow c a) (pow b a)) (pow (* c b) a))
 (rules-127 (pow c (+ b a)) (* (pow c a) (pow c b)))
 (rules-128 (* (pow c a) (pow c b)) (pow c (+ b a)))
 (rules-129 (sqrt (pow b a)) (pow (sqrt b) a))
 (rules-130 (pow (sqrt b) a) (sqrt (pow b a)))
 (rules-131 (pow (cbrt b) a) (cbrt (pow b a)))
 (rules-132 (cbrt (pow b a)) (pow (cbrt b) a))
 (rules-133 (pow b a) (pow (exp a) (log b)))
 (rules-134 (pow (exp a) (log b)) (pow b a))
 (rules-135 (cbrt (* b a)) (* (cbrt a) (cbrt b)))
 (rules-136 (* (cbrt a) (cbrt b)) (cbrt (* b a)))
 (rules-137 (sqrt (* b a)) (* (sqrt a) (sqrt b)))
 (rules-138 (* (sqrt a) (sqrt b)) (sqrt (* b a)))
 (rules-139 (exp (+ b a)) (* (exp b) (exp a)))
 (rules-140 (* (exp b) (exp a)) (exp (+ b a)))
 (rules-141 (+ (log b) (log a)) (log (* b a)))
 (rules-142 (log (* b a)) (+ (log b) (log a)))
 (rules-143 (log (sqrt (* b a))) (* 1/2 (log (* b a))))
 (rules-144 (* 1/2 (log (* b a))) (log (sqrt (* b a))))
 (rules-145 (log (cbrt (pow b a))) (* 1/3 (log (pow b a))))
 (rules-146 (* 1/3 (log (pow b a))) (log (cbrt (pow b a))))
 (rules-148 (log (exp a)) a)
 (rules-149 (pow a 1) a)
 (rules-151 (pow 1 a) 1)
 (rules-152 (exp 0) 1)
 (rules-154 (log 1) 0)
 (rules-156 (cbrt 1) 1)
 (rules-158 (sqrt 1) 1)
 (rules-161 (exp (log a)) a)
 (rules-162 (exp (neg a)) (/ 1 (exp a)))
 (rules-163 (/ 1 (exp a)) (exp (neg a)))
 (rules-164 (pow a b) (exp (* b (log a))))
 (rules-165 (exp (* b (log a))) (pow a b))
 (rules-166 (sqrt a) (pow a 1/2))
 (rules-167 (pow a 1/2) (sqrt a))
 (rules-168 (cbrt a) (pow a 1/3))
 (rules-169 (pow a 1/3) (cbrt a)))

(define-ruleset*
 ruler-non-simplify
 (ruler arithmetic)
 #:type
 ((a real) (b real) (c real))
 (rules-55 a (neg (neg a)))
 (rules-57 a (- a 0))
 (rules-59 a (* a 1))
 (rules-61 a (/ a 1))
 (rules-63 a (+ a 0))
 (rules-103 -1 (cos (PI)))
 (rules-105 0 (sin (PI)))
 (rules-107 1 (cos (+ (PI) (PI))))
 (rules-109 1 (cos 0))
 (rules-111 0 (tan 0))
 (rules-113 0 (sin 0))
 (rules-115 1 (sin (/ (PI) 2)))
 (rules-117 0 (cos (/ (PI) 2)))
 (rules-147 a (log (exp a)))
 (rules-150 a (pow a 1))
 (rules-153 1 (exp 0))
 (rules-155 0 (log 1))
 (rules-157 1 (cbrt 1))
 (rules-159 1 (sqrt 1))
 (rules-160 a (exp (log a))))
