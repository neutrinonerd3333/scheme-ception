#lang racket
;;
;; test-eval.scm
;;
(require r5rs)
(require rackunit)
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

;; ==== QUESTION 1 ====

(test-case
 "new primitives"

 (define test-env (setup-environment))
 (define (test-eval exp) (m-eval exp test-env))

 (check-equal? 5 (test-eval '(/ 15 3))
               "/ primitive")
 (check-equal? 221 (test-eval '(* 13 17))
               "* primitive")
 (check-true (test-eval '(>= 3 3))
             ">= primitive")
 (check-false (test-eval '(<= (+ 1 2) (- 9 100)))
             "<= primitive")
 (check-equal? (list 'foo 'bar 'baz) (test-eval (quote (list 'foo 'bar 'baz)))
               "list primitive")
 (check-equal? 23 (test-eval '(cadr (list 19 23 29)))
               "cadr primitive")
 (check-equal? '(29) (test-eval '(cddr (list 19 23 29)))
               "cddr primitive")
 (check-equal? 3 (test-eval '(length (list 19 23 29)))
               "length primitive")
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
 (check-equal? (void) (test-eval '(void 1 2 3 4 #t #f (quote sdfsfds)))
               "void primitive")
 )

(test-case
 "Printing primitives"

 (define test-print-env (setup-environment))
 (define (check-print str exp)
  (define out (open-output-string))
  (parameterize ((current-output-port out))
    (m-eval exp test-print-env))
  (check-equal? str (get-output-string out)))

 (check-print "\n" '(newline))
 (check-print "Hello\n world 3333" '(printf "Hello\n world ~a" 3333))
 )

;; ==== QUESTION 2 ====

(test-case
 "and special form"
 (define the-test-env (setup-environment))
 (define (test-eval exp) (m-eval exp the-test-env))

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
 (m-eval '(define (counter n)
  (lambda ()
    (set! n (+ n 1))
    n)) the-test-env)
 (test-eval '(define ctr (counter 0)))
 (check-false (test-eval '(and #f (ctr))))
 (check-equal? 1 (test-eval '(ctr))
               "and should short-circuit")
 )


;; ==== QUESTION 3 ====

(test-case
  "until special form"
  (define the-test-env (setup-environment))
  (define (test-eval exp) (m-eval exp the-test-env))

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
  (define test-env (setup-environment))
  (define (test-eval exp) (m-eval exp test-env))

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

  (define test-env (setup-environment))
  (define (test-eval exp) (m-eval exp test-env))

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
  ;; # variables: 0, 1, many
  (check-equal? '()
                (test-eval '(get-empty-env))
                "there should be no variables defined")
  (check-equal? '(i-am-a-variable)
                (test-eval '(get-one-elt-env))
                "there should be one variable defined")
  (check-not-false (member 'car (test-eval '(current-env)))
                   "global env should contain bindings for primitives")
  (check-equal? '(init)
                (test-eval '(procedure-env my-counter))
                "check procedure env")


  ;; env-parent argument artitions
  ;; environment: empty, global, sub-global

  (check-equal? (test-eval '(current-env))
                (test-eval '(get-empty-env))
                "parent of sub-environment of global env is global env")
  (check-equal? '()
                (test-eval '(env-variables (env-parent (current-env))))
                "empty env (parent of global env) is empty")
  (check-exn exn:fail?
             (lambda ()
                     (test-eval '(env-parent (env-parent (current-env)))))
             "empty env (parent of global env) has no parent") ;; like batman


  ;; env-value argument partitions
  ;; environment: empty, global, sub-global
  ;; number of bindings in top-frame of env: 0, 1, many
  ;; symbol is bound? #t/#f
  ;; binding type: primitive or not
  ;; binding location: top frame, sub-frames

  ;; empty env, 0 bindings, unbound symbol
  (check-false (test-eval '(env-value (env-parent (current-env))
                                      (quote i-am-not-bound))
               "env-value returns false for unbound symbols")

  ;; global env, many bindings, bound symbol, is primitive
  (check-equal? (test-eval '(cdr))
                (test-eval '(env-value (current-env) (quote cdr)))
                "primitives are bound in the global env")

  ;; sub-global env, 1 binding, bound symbol, not primitive
  (check-equal? 37
                (test-eval '(env-value (get-one-elt-env)
                                       (quote i-am-a-variable)))
                "bindings in non-global envs")

  ;; sub-global env, 1 binding, bound symbol, primitive, not-top frame
  (check-equal? (test-eval '(list))
                (test-eval '(env-value (get-one-elt-env) (quote list)))
                "env-value can access bindings not in top-most frame")
  )


(display "Done running tests.")(newline)
