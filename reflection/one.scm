
;;; what is this I don't even

(define notvalof
  (lambda (exp env k)
    (if (not (pair? exp))
        (if (symbol? exp)
            (k (env exp))
            (k exp))
        (notvalof (car exp) env
               (lambda (proc)
                 (proc rand env k))))))

(trace-define valof
  (lambda (exp env k)
    (if (equal? (cdr exp) '())
        (if (symbol? exp)
            (k (env (car exp)))
            (k (car exp)))
        (if (equal? (car exp) 'reify)
            (k (eval `(lambda ,(cadr exp) ,(caddr exp))))
            (valof (car exp) env (lambda (proc) (proc (cdr exp) env k)))))))

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

