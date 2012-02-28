;;; Aaron Todd, Advanced Dan, Spring 2012

;;; A delimited continuation implementation
;;;   - Allows for arbitrary numbers of shifts and resets
;;;   - If an id is given to shift it jumps to the reset with corresponding depth
;;;   - If no id is given it pairs with the nearest reset
;;;   - continuations can be moved to the "outside world" and invoked without issue (was the goal...so?)

(define reset-id 0)
(define rks '())
(define jks '())
(define shift-depth 0)
(define shifted '())

(define-syntax shift
  (syntax-rules ()
    [(_ body) (fshift body)]
    [(_ n body) (fshift n body)]))

(define fshift
  (case-lambda
    [(n f) 'hukarz]
    [(f) 'hukarz]))

(define-syntax reset
  (syntax-rules ()
    [(_ body)
     (call/cc (lambda (jk)
                (fluid-let ([reset-id (add1 reset-id)])
                  (fluid-let ([rks (cons (cons reset-id (box '())) rks)]
                              [jks (cons (cons reset-id (box `(,jk))) jks)])
                    ((call/cc (lambda (uk)
                                (let* ([push-rk (lambda (id k) (call/cc (lambda (outk)
                                                                           (uk (lambda () (set-box! (cdr (assoc id rks)) (cons k (unbox (cdr (assoc id rks))))) (outk))))))]
                                       [push-jk (lambda (id k) (call/cc (lambda (outk)
                                                                           (uk (lambda () (set-box! (cdr (assoc id jks)) (cons k (unbox (cdr (assoc id jks))))) (outk))))))]
                                       [apply-jk (lambda (id ebody) (if (not (null? (unbox (cdr (assoc id jks)))))
                                                                        (let ([ks (unbox (cdr (assoc id jks)))])
                                                                          (set-box! (cdr (assoc id jks)) (cdr ks))
                                                                          ((car ks) ebody))
                                                                        (jk ebody)))]
                                       [apply-rk (lambda (id ebody)  (if (not (null? (unbox (cdr (assoc id rks)))))
                                                                         (let ([ks (unbox (cdr (assoc id rks)))])                                                                           
                                                                           (set-box! (cdr (assoc id rks)) (cdr ks))                                                                         
                                                                           ((car ks) ebody))
                                                                         (apply-jk id ebody)))])
                                  
                                  
                                  
                                  (fluid-let ([fshift (case-lambda 
                                                              [(id f)
                                                               (let ([sid (gensym)])
                                                                 (fluid-let ([shift-depth (add1 shift-depth)]
                                                                             [shifted (cons (cons sid (box #f)) shifted)])
                                                                   (call/cc (lambda (k)
                                                                              (apply-jk id (f (lambda (arg)
                                                                                                (call/cc (lambda (ik)
                                                                                                           (if (and (> shift-depth 1) (not (unbox (cdr (assoc sid shifted)))))
                                                                                                               (begin (set-box! (cdr (assoc sid shifted)) #t) (push-jk id ik))
                                                                                                               (push-rk id ik))
                                                                                                           (k arg))))))))))]
                                                              [(f) (let ([nid reset-id]) (fshift nid f))])])
                                                    (let ([ebody body])
                                                      (lambda () (apply-rk reset-id ebody))))))))))))]))


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
                          (add1 
                           (reset
                            (bytevector=? (make-bytevector (* 100 MB) (random 255))
                                          (shift 1 (lambda (k)
                                                     (k (make-bytevector (* 100 MB) (random 255)))
                                                     4)))))
                          (loop (sub1 n)))]))])
      (loop x))))

;;; Basic tests

(load "../utils/tester.scm")

(define run-tests
  (lambda ()

    (test-check "identity k"
      (reset (shift (lambda (k) (k 120))))
      120)

    (test-check "apply k twice"
      (reset (+ 10 (shift (lambda (k) (k (k 100))))))
      120)

    (test-check "dynamic, first reset"
      (reset (+ 100 (reset (+ 10 (shift (lambda (k) (k (k 0))))))))
      120)

    (test-check "dynamic numeric id 1"
      (reset (+ 50 (reset (+ 10 (shift 1 (lambda (k) (k (k 0))))))))
      120)

    (test-check "dynamic numeric id > 1"
      (reset (+ 100 (reset (+ 10 (shift 2 (lambda (k) (k (k 0))))))))
      120)

    (test-check "k outside reset"
      (let ([madd (reset (+ 10 (shift (lambda (k) k))))])
        (reset (+ 100 (shift (lambda (k) (madd (k (madd 0))))))))
      120)

    (test-check "shift without reset in function"
      (let ([twice (lambda (x) (shift (lambda (k) (k (k x)))))])
        (reset (+ 10 (twice 100))))
      120)

    (test-check "shift outside reset returning k outside reset"
      (let* ([twice (lambda () (shift (lambda (k) (lambda (x) (k (k x))))))]
             [madd (reset (+ 10 (twice)))])
        (madd 100))
      120)

    (test-check "nested shifts targeting same reset"
      (reset (+ 5 (shift (lambda (k)
                           (k (k (reset (+ 55 (shift (lambda (kk)
                                                         (kk (kk 0))))))))))))
      120)
      
    ))