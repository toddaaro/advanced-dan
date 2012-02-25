
;;; An implementation of delimited continuations

;(define top
;  ((call/cc (lambda (k) k))

;;; A delimited continuation implementation
;;;   - Allows for arbitrary numbers of shifts and resets
;;;   - If an id is given to shift it jumps to the reset with corresponding depth
;;;   - If no id is given it pairs with the nearest reset

(define reset-id 0)
(define rks '())
(define jks '())

(define update-rk
  (lambda (id k)
    (set-box! (cdr (assoc id rks)) k)))

(define-syntax reset
  (syntax-rules ()
    [(_ body)
     (call/cc (lambda (jk)
                (fluid-let ([reset-id (add1 reset-id)])
                  (fluid-let ([rks (cons (cons reset-id (box jk)) rks)]
                              [jks (cons (cons reset-id jk) jks)])
                      (fluid-let-syntax ([shift (syntax-rules ()
                                                  [(_ id f)
                                                   (call/cc (lambda (k)
                                                              ((cdr (assoc id jks)) (f (lambda (arg)
                                                                                         (call/cc (lambda (ik)
                                                                                                    (update-rk id ik)
                                                                                                    (k arg))))))))]
                                                  [(_ f) (shift reset-id f)])])
                                        (let ([ebody body])
                                          ((unbox (cdr (assoc reset-id rks))) ebody)))))))]))
