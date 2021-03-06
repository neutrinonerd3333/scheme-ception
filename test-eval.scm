#lang racket
;;
;; test-eval.scm
;;
(require r5rs)
(require rackunit)
(require rackunit/text-ui)
(require "eval.scm")

;;
;;                       _oo0oo_
;;                      o8888888o
;;                      88" . "88
;;                      (| -_- |)
;;                      0\  =  /0
;;                    ___/`---'\___
;;                  .' \\|     |// '.
;;                 / \\|||  :  |||// \
;;                / _||||| -:- |||||- \
;;               |   | \\\  -  /// |   |
;;               | \_|  ''\---/''  |_/ |
;;               \  .-\__  '-'  ___/-. /
;;             ___'. .'  /--.--\  `. .'___
;;          ."" '<  `.___\_<|>_/___.' >' "".
;;         | | :  `- \`.;`\ _ /`;.`/ - ` : | |
;;         \  \ `_.   \_ __\ /__ _/   .-` /  /
;;     =====`-.____`.___ \_____/___.-`___.-'=====
;;                       `=---='
;;
;;    This code protected from bugs by code Buddha
;;

(define (test-suite-with test-eval name)
  (test-suite name
  ;; ==== QUESTION 1 ====

  (test-case
   "new primitives"

   (check-equal? 221 (test-eval '(* 13 17))
                 "* primitive")
   (check-equal? 5 (test-eval '(/ 15 3))
                 "/ primitive")
   (check-true (test-eval '(>= 3 3))
               ">= primitive")
   (check-false (test-eval '(<= (+ 1 2) (- 9 100)))
               "<= primitive")

   (check-true (test-eval '(eq? 1 1))
               "eq? primitive (true)")
   (check-false (test-eval '(eq? (list 1 2 3) (list 1 2 3)))
                "eq? primitive (false)")
   (check-true (test-eval '(equal? (list 1 2) (list 1 2)))
               "equal? primitive (true)")
   (check-false (test-eval '(equal? (list 1 3) (list 1 2)))
                "equal? primitive (false)")

   (check-equal? (list 'foo 'bar 'baz) (test-eval '(list 'foo 'bar 'baz))
                 "list primitive")

   (check-equal? 23 (test-eval '(cadr (list 19 23 29)))
                 "cadr primitive")
   (check-equal? 23 (test-eval '(caadr (list 19 (list 23 29))))
                 "caadr primitive")
   (check-equal? '(29) (test-eval '(cdadr (list 19 (list 23 29))))
                 "cdadr primitive")

   (check-equal? '(29) (test-eval '(cddr (list 19 23 29)))
                 "cddr primitive")
   (check-equal? 29 (test-eval '(caddr (list 19 23 29)))
                 "caddr primitive")
   (check-equal? 31 (test-eval '(cadddr (list 19 23 29 31)))
                 "cadddr primitive")

   (check-equal? 19 (test-eval '(first (list 19 23 29)))
                 "first primitive")
   (check-equal? 23 (test-eval '(second (list 19 23 29)))
                 "second primitive")
   (check-equal? 29 (test-eval '(third (list 19 23 29)))
                 "third primitive")
   (check-equal? 31 (test-eval '(fourth (list 19 23 29 31)))
                 "fourth primitive")
   (check-equal? '(23 29 31) (test-eval '(rest (list 19 23 29 31)))
                 "rest primitive")

   (check-equal? 3 (test-eval '(length (list 19 23 29)))
                 "length primitive")
   (check-equal? '(12 34 56 78 90)
                 (test-eval '(append (list 12 34) (list 56) (list 78 90)))
                 "append primitive")

   (check-true (test-eval '(number? 3))
               "number? primitive (true)")
   (check-false (test-eval '(number? (list (quote a-symbol))))
                "number? primitive (false)")
   (check-true (test-eval '(string? "i am a string"))
               "string? primitive (true)")
   (check-false (test-eval '(string? (list (quote a-symbol))))
                "string? primitive (false)")
   (check-true (test-eval '(boolean? #t))
               "boolean? primitive (true)")
   (check-false (test-eval '(boolean? (list (quote a-symbol))))
                "boolean? primitive (false)")
   (check-true (test-eval '(symbol? (quote i-am-a-symbol)))
               "symbol? primitive (true)")
   (check-false (test-eval '(symbol? (list (quote a-symbol))))
                "symbol? primitive (false)")
   (check-true (test-eval '(pair? (list 109 119)))
               "pair? primitive (true)")
   (check-false (test-eval '(pair? (quote herpy-derp-not-a-pair)))
                "pair? primitive (false)")

   (check-equal? (void) (test-eval '(void 1 2 3 4 #t #f (quote sdfsfds)))
                 "void primitive")
   (check-not-exn (lambda () (test-eval '(gensym)))
                  "gensym primitive")

   (define immutable-box (test-eval '(box-immutable (list 12 34567))))
   (define unbox-box (test-eval '(unbox (box-immutable (list 12 34567)))))
   (check-true (and (box? immutable-box) (immutable? immutable-box))
               "box-immutable primitive")
   (check-equal? '(12 34567) unbox-box
                 "unbox primitive")
   )

  (test-case
   "Printing primitives"

   (define (check-print str exp)
    (define out (open-output-string))
    (parameterize ((current-output-port out))
      (test-eval exp))
    (check-equal? str (get-output-string out)))

   (check-print "\n" '(newline))
   (check-print "Hello\n world 3333" '(printf "Hello\n world ~a" 3333))
   )

  ;; ==== QUESTION 2 ====

  (test-case
   "and special form"

   ;; partitions
   ;; num args: 0, 1, >1
   ;; return value: #f, #t, other stuff
   ;; last value is deciding?
   ;; subexpressions?

   (check-true (test-eval '(and))
               "and with 0 args gives #t")
   (check-true (test-eval '(and #t))
               "(and #t) = #t")
   (check-false (test-eval '(and #f))
                "(and #f) = #f")
   (check-equal? '() (test-eval '(and '()))
                 "and with 1 arg gives the arg, not necessarily bool")

   (check-true (test-eval '(and #t #t #t #t)))
   (check-true (test-eval '(and #t #t (quote foo) #t)))
   (check-false (test-eval '(and #t #t #t #f)))

   ;; more involved
   (check-equal? 12212 (test-eval '(and 121241421 1231241 12212))
                 "if no arg evaluates to #f, and gives last arg")
   (check-equal? 7 (test-eval '(and (+ 3 4)))
                 "and returns *result* of last arg if true")
   (check-false (test-eval '(and (< 1 2) (= 5 5) (> 4 5)))
                "and returns *result* of evaluating last arg if it's #f")
   (check-true (test-eval '(and (< 1 2) (+ 3 (+ 1 2)) (= 5 5))))

   (test-eval '(define x -123))
   (check-true (test-eval '(and (< x 4) (= 5 5) (null? '())))
               "and should be ok with variables in clauses")

   ;; if we defined a procedure "and", we wouldn't get short circuiting, BAD
   (test-eval '(define (counter n)
    (lambda ()
      (set! n (+ n 1))
      n)))
   (test-eval '(define ctr (counter 0)))
   (check-false (test-eval '(and #f (ctr))))
   (check-equal? 1 (test-eval '(ctr))
                 "and should short-circuit")
   )

  ;; ==== QUESTION 3 ====

  (test-case
    "until special form"

    ;; partitions
    ;; times loop is run: 0, 1, many
    ;; number of expressions in body: 0, 1, many

    (test-eval '(define x 0))
    (test-eval '(define y 0))
    (test-eval '(define (incr-x) (set! x (+ 1 x))))
    (test-eval '(define (decr-y) (set! y (- y 1))))

    (check-true (void? (test-eval '(until (= 0 0))))
                "until has no return value (even with no exprs in body)")

    (check-true (void? (test-eval '(until (>= x 0) (incr-x))))
                "until has no return value")
    (check-equal? 0 (test-eval 'x)
                  "until should not have run its body")

    (check-true (void? (test-eval '(until (>= x 1) (incr-x))))
                "until returns void")
    (check-equal? 1 (test-eval 'x)
                  "until should have run exactly once")

    ;; x is currently 1

    ;; multi-expr body
    (check-true (void? (test-eval '(until (>= x (+ 50 50)) (incr-x) (decr-y))))
                "until returns void")
    (check-equal? 100 (test-eval 'x)
                  "x should now be 100")
    (check-equal? -99 (test-eval 'y)
                  "y should now be -99")
    )

  ;; ==== QUESTION 4 ====

  (test-case
    "unset! special form"

    ;; partitions
    ;; # times set! called: 0, 1, many
    ;; unset - set: -, 0, +
    ;; location of unset binding: global env, subenv

    (check-exn exn:fail?
               (lambda () (test-eval '(unset! i-am-not-defined)))
               "unset! should fail on undefined variables")

    (test-eval '(define x 1337))
    (test-eval '(unset! x))
    (check-equal? 1337 (test-eval 'x)
      "unset! is idempotent on unmutated variables")

    (test-eval '(define x (quote asian-dance-team)))
    (test-eval '(unset! x))
    (check-equal? 'asian-dance-team (test-eval 'x)
      "unset! cannot look past a define")

    (test-eval '(set! x 83))
    (test-eval '(set! x 89))
    (test-eval '(unset! x))
    (check-equal? 83 (test-eval 'x)
      "unset! undoes last not-undone set!")

    (test-eval '(set! x 101))
    (test-eval '(unset! x))
    (check-equal? 83 (test-eval 'x)
      "unset! undoes last not-undone set! (not necessarily lest set!)")

    (test-eval '(define f (lambda (x) (lambda () (set! x 131) (unset! x) x))))
    (check-equal? 405 (test-eval '((f 405)))
      "unset! should work in not-global environements")
    )

  ;; ==== QUESTION 5 ====

  (test-case
    "environments"

    (test-eval '(define get-empty-env
               (lambda () (current-env))))
    (test-eval '(define get-one-elt-env
               (lambda ()
                       (let ((i-am-a-variable 37))
                            (current-env)))))

    (test-eval '(define make-counter
                        (lambda (init)
                                (lambda ()
                                        (set! init (+ init 1))
                                        init))))
    (test-eval '(define my-counter (make-counter 0)))

    ;; env-variables argument partitions
    ;; environment: global, not
    ;; # variables in top-most frame: 0, 1, many
    (check-equal? '()
                  (test-eval '(env-variables (get-empty-env)))
                  "there should be no variables defined")
    (check-equal? '(i-am-a-variable)
                  (test-eval '(env-variables (get-one-elt-env)))
                  "there should be one variable defined")
    (check-not-false (member 'car
                             (test-eval '(env-variables (current-env))))
                     "global env should contain bindings for primitives")
    (check-equal? '(init)
                  (test-eval '(env-variables (procedure-env my-counter)))
                  "check procedure env")


    ;; env-parent argument artitions
    ;; environment: global, sub-global

    (check-equal? (test-eval '(current-env))
                  (test-eval '(env-parent (get-empty-env)))
                  "parent of sub-environment of global env is global env")
    (check-exn exn:fail?
               (lambda ()
                       (test-eval '(env-parent (current-env))))
               "global env has no parent")
               ;; (much like batman)


    ;; env-value argument partitions
    ;; environment: global, sub-global
    ;; number of bindings in top-frame of env: 0, 1, many
    ;; symbol is bound? #t/#f
    ;; binding type: primitive or not
    ;; binding location: top frame, sub-frames

    ;; sub-global env, 0 bindings, unbound symbol
    (check-false (test-eval '(env-value (quote i-am-not-bound)
                                        (get-empty-env)))
                 "env-value returns false for unbound symbols")

    ;; global env, many bindings, bound symbol, is primitive
    (check-equal? (test-eval 'cdr)
                  (test-eval '(env-value (quote cdr) (current-env)))
                  "primitives are bound in the global env")

    ;; sub-global env, 1 binding, bound symbol, not primitive
    (check-equal? 37
                  (test-eval '(env-value (quote i-am-a-variable)
                                         (get-one-elt-env)))
                  "bindings in non-global envs")

    ;; sub-global env, 1 binding, bound symbol, primitive, not-top frame
    (check-equal? (test-eval 'list)
                  (test-eval '(env-value (quote list) (get-one-elt-env)))
                  "env-value can access bindings not in top-most frame")
    )
))


;; these are the vanilla tests running in m-eval
(define test-env (setup-environment))
(define (single-depth-test-eval exp) (m-eval exp test-env))
(define single-depth-suite
  (test-suite-with single-depth-test-eval "single-depth tests"))

;; ==== QUESTION 6 ====
;; these tests run the same commands as before, except one level deeper
;; *cue "Inception" boom*

(load-meval-defs)
(m-eval '(define second-level-env (setup-environment)) the-global-environment)
(define (double-depth-test-eval exp) (m-eval `(m-eval (quote ,exp) second-level-env) the-global-environment))
(define depth-two-suite
  (test-suite-with double-depth-test-eval "depth-two tests"))

(display "Running tests on m-eval")(newline)
(run-tests single-depth-suite)
(display "Running tests on m-eval running inside m-eval")(newline)
(run-tests depth-two-suite)

(display "Done running tests.")(newline)
