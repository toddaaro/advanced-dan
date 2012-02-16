
;;; Aaron Todd
;;; B621 prog macros

;; turn an imperative program with gotos and set!s into a scheme program using macros.

;;; Version 1

(define-syntax prog
  (syntax-rules ()
    [(_ goto return args main exps ...)
     ((call/cc (lambda (k)
                 (set! goto k)
                 (set! return (lambda (x) (k (lambda () x))))
                 (prex main () exps ...))))]))

(define-syntax prex
  (syntax-rules ()
    [(_ main (out ...) (id b ...))
     (letrec
         (out ... (id (lambda () b ...)))
         (main))]
    [(_ main (out ...) (id b ...) (ni n ...) rest ...)
     (prex main (out ... (id (lambda () b ... (ni)))) (ni n ...) rest ...)]))

(define progi
  '(prog goto return (x y z) x^
     (x^
       (set! x 5)
       (set! y x))
     (y^
       (set! z (+ 3 y))
       (goto a^))
     (z^
       (return 120))
     (a^
       (return z))
   )
)

(define loopi
  '(prog goto return (x y z) x^
          (x^
           (goto y^))
          (y^
           (goto x^)
           (return 120))))

;;; Version 2

;; updated with in class feedback
;;   - let-syntax
;;   - goto instead of k
;;   - newline elimination optmization

(define-syntax progs
  (syntax-rules ()
    [(_ goto return args main (out ...) (id b ...))
     ((call/cc (lambda (goto)
                 (let-syntax ([return (syntax-rules () [(_ e) (goto (lambda () e))])])
                 (letrec (out ... (id (lambda () b ...))) (main))))))]
    [(_ goto return args main (out ...) (id b ...) (ni n ...) rest ... )
     (progs goto return args main (out ... (id (lambda () b ... ni))) (ni n ...) rest ...)]))

(define progsi
  '(progs goto return (x y z) x^ ()
          (x^
           (set! x 5)
           (set! y x))
          (y^
           (set! z (+ 3 y))
           (goto a^))
          (z^
           (return 120))
          (a^
           (return z))))

(define loopsi
  '(progs goto return (x y z) x^ ()
          (x^
           (goto y^))
          (y^
           (goto x^)
           (return 120))))






