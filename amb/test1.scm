
(define run
  (lambda ()
    (let (;[gk 'hukarz]
          [eng (make-engine (lambda ()
                              (+ 5 
                                 (call/cc (lambda (k)
                                            (set! gk k)
                                            (engine-return k 'hukarz)
                                            120)))))])
      (eng 10000
           (lambda (ticks . (cont . values))
             (list ticks values
                   (let ([e (make-engine (lambda () (cont 1)))])
                     (e 25 list (lambda (x) x)))))
           (lambda (x) x)))))
                      
                  

(define run2
  (lambda ()
    (let ([eng (make-engine (lambda ()
                              (+ 5 (begin
                                     (engine-block)
                                     120))))])
      (eng 100
           list
           (lambda (x)
             (call/cc (lambda (k)
                      (k 1))))))))