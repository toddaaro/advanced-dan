
;; Why does the following run?
;; More specifically, how does x know that in the future it will point to a box?

(define run
  (lambda ()
    (x (call/cc (lambda (k)
                  (set! x (lambda (h) 120))
                  (k 'hukarz))))))

;; answer: evaluation-order dependent behavior

