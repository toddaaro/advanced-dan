
;;; what is this I don't even


(trace-define valof1
  (lambda (exp env k)
    (if (equal? (cdr exp) '())
        (if (symbol? exp)
            (k (env (car exp)))
            (k (car exp)))
        (if (equal? (car exp) 'reify)
            (k (eval `(lambda ,(cadr exp) ,(caddr exp))))
            (valof1 (car exp) env (lambda (proc) (proc (cdr exp) env k)))))))


(define-syntax tif
  (syntax-rules ()
    [(_ t e1 e2)
     (if (begin (display t) (newline) t)
         (begin (display e1) (newline) e1)
         (begin (display e2) (newline) e2))]))

(trace-define valof
              (lambda (exp env k)
                (tif (not (pair? exp))
                     (tif (symbol? exp)
                          (k (env exp))
                          (k exp))
                     (tif (equal? (car exp) 'reify)
                          (valof `(lambda ,(cadr exp) ,(caddr exp)) env k)
                          (valof (car exp) env (lambda (proc) (proc (cdr exp))))))))

;;; environment

(define-syntax bottom-env
  (syntax-rules ()
    [(_)
     (lambda (x)
       (if (equal? x 'lambda)
           (lambda (e* env k)
             (display e*)
             (newline)
             (eval 
              `(let ([,(cadar e*) ,env]
                     [,(caddar e*) ,k])
                 (lambda ,(caar e*) ,(cadr e*)))))
           '()))]))

;;;

(trace-define val
  (lambda (exp env k)
    (if (not (pair? exp))
        (if (symbol? exp)
            (k (env exp))
            (k exp))
        (if (equal? (car exp) 'reify)
            (val `(lambda ,(cadr exp) ,(caddr exp)) env k)
            (val (car exp) env (lambda (proc) (proc (cdr exp) env k)))))))

(define-syntax bot-env
  (syntax-rules ()
    [(_)
     (lambda (x)
       (if (equal? x 'lambda)
           (lambda (e* env k)
             (k (eval `(lambda ,(car e*) ,(cadr e*)))))))]))

(define-syntax run
  (syntax-rules ()
    [(_ exp)
     (val exp (bot-env) (lambda (x) x))]))

;; this is the reify line for the next interpreter
;; (valof `(reify ,(cadr exp) ,(caddr exp)) env k)


;; app line
; (valof (car exp) env (lambda (proc) (proc (cdr exp) env k)))

;; reify line
; (lambda (x env k) body)

;; realization!
;; valof will make the lambda!
;;(reify (e env k) body)

;;(valof (lambda (e env k) body) env k)


;; what is in the environment?
;; 

;; what are the options for lines
;; 1) application
;; 2) reify
;; 3) environment lookup

;; I think environment and application are the two I need
;; reify can be a function inside the environment, or built into app?

