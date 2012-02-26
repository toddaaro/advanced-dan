;;; Aaron Todd, Advanced Dan, Spring 2012

;;; A delimited continuation implementation
;;;   - Allows for arbitrary numbers of shifts and resets
;;;   - If an id is given to shift it jumps to the reset with corresponding depth
;;;   - If no id is given it pairs with the nearest reset
;;;   - continuations can be moved to the "outside world" and invoked without issue (was the goal...so?)

(define reset-id 0)
(define rks '())
(define jks '())

(define-syntax reset
  (syntax-rules ()
    [(_ body)
     (call/cc (lambda (jk)
                (fluid-let ([reset-id (add1 reset-id)])
                  (fluid-let ([rks (cons (cons reset-id (box jk)) rks)]
                              [jks (cons (cons reset-id jk) jks)])
                    ((call/cc (lambda (uk)
                                (let ([update-rk (lambda (id k) (call/cc (lambda (outk)
                                                                           (uk (lambda () (set-box! (cdr (assoc id rks)) k) (outk))))))])
                                  (fluid-let-syntax ([shift (syntax-rules ()
                                                              [(_ id f)
                                                               (call/cc (lambda (k)
                                                                          ((cdr (assoc id jks)) (f (lambda (arg)
                                                                                                     (call/cc (lambda (ik)
                                                                                                                (update-rk id ik)
                                                                                                                (k arg))))))))]
                                                              [(_ f) (let ([nid reset-id]) (shift nid f))])])
                                                    (let ([ebody body])
                                                      (lambda () ((unbox (cdr (assoc reset-id rks))) ebody))))))))))))]))


;;; Tests
;;;  - Need to test the hypothesis that the fluid-lets eliminate memory leaks due to global data strutures
;;;  - These appear to confirm it,
;;;    - If (collect (collect-maximum-generation)) is called after a run there is no net memory consumption increase


(define MB (expt 2 20))

(define test-mem
  (lambda (x)
    (letrec ([loop (lambda (n)
                     (cond
                       [(zero? n) 120]
                       [else
                        (begin (reset
                                (bytevector=? (make-bytevector (* 100 MB) (random 255)) (shift (lambda (k)
                                                   (k (make-bytevector (* 100 MB) (random 255)))))))
                               (loop (sub1 n)))]))])
      (loop x))))
                     
(define test-mem2
  (lambda (x)
    (letrec ([loop (lambda (n)
                     (cond
                       [(zero? n) 120]
                       [else
                        (begin
                          (reset
                           (or #f
                               (reset
                                (bytevector=? (make-bytevector (* 100 MB) (random 255))
                                              (shift 1 (lambda (k)
                                                         (k (make-bytevector (* 100 MB) (random 255)))))))))
                          (loop (sub1 n)))]))])
      (loop x))))

