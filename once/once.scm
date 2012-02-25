
;;; A macro that makes it so if a function is called inside the macro it becomes call by need

(define computed '())

(define-syntax once
  (syntax-rules ()
    [(_ e)
     (if (assoc 'e computed)
         (cdr (assoc 'e computed))
         (let ([r e]) 
           (set! computed (cons `(e . ,r) computed))
           r))]))

(define ex1
  (lambda ()
    (once (fact 5))))

(define f
  (lambda (n)
    (once (fact n))))

(define fact
  (lambda (n)
    (cond
      [(eq? 0 n) 1]
      [else (* n (fact (- n 1)))])))