(load "prog.scm")
(print-gensym 'pretty/suffix)

(define call/cc-inc
  '(progs goto return (x y z) x^ ()
          (x^
           (set! x (+ 5 (call/cc (lambda (k) (set! y k) 120)))))
          (y^                       
           (y x)
           (return x))))

(define expanded
  (lambda ()
    (expand call/cc-inc)))

(define evaled
  (lambda ()
    (eval call/cc-inc)))

(define let-added
  '(let ([x 'hukarz] [y 'hukarz])
    ((#2%call/cc
      (lambda (goto.0)
        (letrec ([x^.1 (lambda ()
                         (set! x
                               (#2%+
                                5
                                (#2%call/cc (lambda (k.2) (set! y k.2) 120))))
                         y^.3)]
                 [y^.3 (lambda () (y x) (goto.0 (lambda () x)))])
          (x^.1)))))))
  