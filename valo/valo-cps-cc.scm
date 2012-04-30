
;;; A basic scheme interpreter in miniKanren

;;(load "ck.scm")
;;(load "neq.scm")
;;(load "tree-unify.scm")
;;(load "miniKanren.scm")

(import (ck))
(import (neq))

(load "matche.scm")
(load "tester.scm")

(define valo-cps
  (lambda (exp env k val)
    (matche `(,exp ,env ,k ,val)

      [(,exp ,env ,k ,val)
       (conde
         [(numo exp) (apply-k k exp val)]
         [(== exp #t) (apply-k k #t val)]
         [(== exp #f) (apply-k k #f val)]
         [(fresh (var-sym var-val)
            (== exp `(var ,var-sym))
            (apply-envo env var-sym var-val)
            (apply-k k var-val val))])]
      
      [((zero? ,n) ,env ,k ,val)
       (valo-cps n env (zero-k k) val)]

      [((sub1 ,n) ,env ,k ,val)
       (valo-cps n env (sub1-k k) val)]

      [((* ,a ,b) ,env ,k ,val)
       (valo-cps a env (mult-k-1 b env k) val)]

      [((if ,t ,a ,b) ,env ,k ,val)
       (valo-cps t env (if-k a b env k) val)]
      
      [((letcc ,k-id ,body) ,env ,k ,val)
       (fresh (new-env)
         (extend-envo k-id k env new-env)
         (valo-cps body new-env k val))]

      [((throw ,v-exp ,k-exp) ,env ,k ,val)
       (valo-cps k-exp env (cc-k v-exp env) val)]

      [(call/cc ,rator) (valo-cps rator env (ccc-k k) val)]
      
      [((lambda (,x) ,body) ,env ,k ,val)
       (apply-k k (closureo x body env) val)]

      [((,rator ,rand) ,env ,k ,val)
       (valo-cps rator env (proc-k rand env k) val)]
      
      )))

(define apply-k
  (lambda (k-exp exp val)
    (matche `(,k-exp ,exp ,val)

      [((ccc-k ,in-k) ,exp ,val) (apply-proco-cps exp in-k k-exp val)]
            
      [((empty-k) ,exp ,val)
       (== val exp)]
      
      [((zero-k ,in-k) ,exp ,val)
       (fresh (is-zero)
         (conde
           [(== exp '()) (apply-k in-k #t val)]
           [(=/= exp '()) (apply-k in-k #f val)]))]
      
      [((sub1-k ,in-k) ,exp ,val)
       (fresh (nv)
         (minuso exp '(1) nv)
         (apply-k in-k nv val))]

      [((mult-k-1 ,b ,env ,in-k) ,exp ,val)
       (valo-cps b env (mult-k-2 exp in-k) val)]

      [((mult-k-2 ,a ,in-k) ,exp ,val)
        (fresh (res)
          (*o a exp res)
          (apply-k in-k res val))]

      [((if-k ,conseq ,alt ,env ,in-k) ,exp ,val)
       (fresh (res)
         (conde
           [(== exp #t) (valo-cps conseq env in-k val)]
           [(== exp #f) (valo-cps alt env in-k val)]))]

      [((proc-k ,rand ,env ,in-k) ,exp ,val)
       (valo-cps rand env (arg-k exp in-k) val)]

      [((arg-k ,proc ,in-k) ,exp ,val)
       (apply-proco-cps proc exp in-k val)]

      [((cc-k ,v-exp ,env) ,exp ,val)
       (valo-cps v-exp env exp val)]
      
      )))

(define empty-envo
  (lambda (empty-env)
    (== empty-env '())))

(define extend-envo
  (lambda (x a env new-env)
    (== new-env `(,x ,a ,env))))

(define apply-envo
  (lambda (env y val)
    (fresh (x a old-env)
      (== env `(,x ,a ,old-env))
      (conde
        [(== x y) (== val a)]
        [(=/= x y) (apply-envo old-env y val)]))))

(define closureo
  (lambda (x body env)
    `(closure ,x ,body ,env)))

(define apply-proco-cps
  (lambda (closure arg k val)
    (matche `(,closure ,arg ,k ,val)
      [((closure ,x ,body ,env) ,arg ,k ,val)
       (fresh (new-env)
         (extend-envo x arg env new-env)
         (valo-cps body new-env k val))]
      [(,k-proc ,arg ,k ,val)
       (apply-k k-proc arg val)])))
    
(define numo
  (lambda (n)
    (matche n
            [()]
            [(1 . ,digits) (numo digits)]
            [(0 . ,digits) (numo digits)])))

(define ccc-k
  (lambda (in-k)
    `(ccc-k ,in-k)))

(define empty-k
  (lambda ()
    '(empty-k)))

(define zero-k
  (lambda (k-exp)
    `(zero-k ,k-exp)))

(define sub1-k
  (lambda (k-exp)
    `(sub1-k ,k-exp)))

(define mult-k-1
  (lambda (b env k-exp)
    `(mult-k-1 ,b ,env ,k-exp)))

(define mult-k-2
  (lambda (a k-exp)
    `(mult-k-2 ,a ,k-exp)))

(define if-k
  (lambda (conseq alt env k-exp)
    `(if-k ,conseq ,alt ,env ,k-exp)))

(define proc-k
  (lambda (rand env k-exp)
    `(proc-k ,rand ,env ,k-exp)))

(define arg-k
  (lambda (proc in-k)
    `(arg-k ,proc ,in-k)))

(define cc-k
  (lambda (v-exp env)
    `(cc-k ,v-exp ,env)))

;;; MY TESTS!
(define run-tests
  (lambda ()

  (test-check "environments"
    (run* (q)
      (fresh (env)
        (extend-envo 'x '(0 1) '() env)
        (apply-envo env 'x q)))
    '((0 1)))
    
  (test-check "sub1"
    (run* (q)
      (valo-cps '(sub1 (0 1)) '() (empty-k) q))
    '((1)))

  (test-check "*"
    (run* (q)
      (valo-cps '(* (0 1) (0 1)) '() (empty-k) q))
    '((0 0 1)))

  (test-check "zero?-1"
    (run* (q)
      (valo-cps '(zero? ()) '() (empty-k) q))
    '(#t))

  (test-check "zero?-2"
    (run* (q)
      (valo-cps '(zero? (1 0 1)) '() (empty-k) q))
    '(#f))

  (test-check "if-1"
    (run* (q)
      (valo-cps '(if #t (0 1) (1)) '() (empty-k) q))
    '((0 1)))

  (test-check "if-2"
    (run* (q)
      (valo-cps '(if #f (0 1) (1)) '() (empty-k) q))
    '((1)))

  (test-check "lambda-1"
    (run* (q)
      (valo-cps '(lambda (x) (var x)) '() (empty-k) q))
    '((closure x (var x) ())))

  (test-check "lambda-2"
    (run* (q)
      (valo-cps '((lambda (x) (var x)) (0 1)) '() (empty-k) q))
    '((0 1)))

  (test-check "lambda-3"
    (run* (q)
      (valo-cps '((lambda (x) (* (0 1) (sub1 (var x)))) (1 1)) '() (empty-k) q))
    '((0 0 1)))

  (test-check "letcc/throw"
    (run* (q)
      (valo-cps '(* (1 1) (letcc z (* (0 1) (throw (0 0 1) (var z)))))
                '()
                (empty-k)
                q))
    '((0 0 1 1)))
  
  (test-check "self-passing-fact"
    (run* (q)
      (valo-cps '((lambda (f) (((var f) (var f)) (1 0 1)))
              (lambda (f)
                (lambda (n) (if (zero? (var n)) (1) (* (var n) (((var f) (var f)) (sub1 (var n))))))))
            '()
            (empty-k)
            q))
    `(,(build-num 120)))

   ))

;;; IMPORTS ARE HARD!

(define-syntax run1 (syntax-rules () ((_ (x) g0 g ...) (run 1 (x) g0 g ...))))
(define-syntax run2 (syntax-rules () ((_ (x) g0 g ...) (run 2 (x) g0 g ...))))
(define-syntax run3 (syntax-rules () ((_ (x) g0 g ...) (run 3 (x) g0 g ...))))
(define-syntax run4 (syntax-rules () ((_ (x) g0 g ...) (run 4 (x) g0 g ...))))
(define-syntax run5 (syntax-rules () ((_ (x) g0 g ...) (run 5 (x) g0 g ...))))
(define-syntax run6 (syntax-rules () ((_ (x) g0 g ...) (run 6 (x) g0 g ...))))
(define-syntax run7 (syntax-rules () ((_ (x) g0 g ...) (run 7 (x) g0 g ...))))
(define-syntax run8 (syntax-rules () ((_ (x) g0 g ...) (run 8 (x) g0 g ...))))
(define-syntax run9 (syntax-rules () ((_ (x) g0 g ...) (run 9 (x) g0 g ...))))
(define-syntax run10 (syntax-rules () ((_ (x) g0 g ...) (run 10 (x) g0 g ...))))

(define-syntax run11 (syntax-rules () ((_ (x) g0 g ...) (run 11 (x) g0 g ...))))
(define-syntax run12 (syntax-rules () ((_ (x) g0 g ...) (run 12 (x) g0 g ...))))
(define-syntax run13 (syntax-rules () ((_ (x) g0 g ...) (run 13 (x) g0 g ...))))
(define-syntax run14 (syntax-rules () ((_ (x) g0 g ...) (run 14 (x) g0 g ...))))
(define-syntax run15 (syntax-rules () ((_ (x) g0 g ...) (run 15 (x) g0 g ...))))
(define-syntax run16 (syntax-rules () ((_ (x) g0 g ...) (run 16 (x) g0 g ...))))
(define-syntax run17 (syntax-rules () ((_ (x) g0 g ...) (run 17 (x) g0 g ...))))
(define-syntax run18 (syntax-rules () ((_ (x) g0 g ...) (run 18 (x) g0 g ...))))
(define-syntax run19 (syntax-rules () ((_ (x) g0 g ...) (run 19 (x) g0 g ...))))
(define-syntax run20 (syntax-rules () ((_ (x) g0 g ...) (run 20 (x) g0 g ...))))

(define-syntax run21 (syntax-rules () ((_ (x) g0 g ...) (run 21 (x) g0 g ...))))
(define-syntax run22 (syntax-rules () ((_ (x) g0 g ...) (run 22 (x) g0 g ...))))
(define-syntax run23 (syntax-rules () ((_ (x) g0 g ...) (run 23 (x) g0 g ...))))
(define-syntax run24 (syntax-rules () ((_ (x) g0 g ...) (run 24 (x) g0 g ...))))
(define-syntax run25 (syntax-rules () ((_ (x) g0 g ...) (run 25 (x) g0 g ...))))
(define-syntax run26 (syntax-rules () ((_ (x) g0 g ...) (run 26 (x) g0 g ...))))
(define-syntax run27 (syntax-rules () ((_ (x) g0 g ...) (run 27 (x) g0 g ...))))
(define-syntax run28 (syntax-rules () ((_ (x) g0 g ...) (run 28 (x) g0 g ...))))
(define-syntax run29 (syntax-rules () ((_ (x) g0 g ...) (run 29 (x) g0 g ...))))
(define-syntax run30 (syntax-rules () ((_ (x) g0 g ...) (run 30 (x) g0 g ...))))

(define-syntax run31 (syntax-rules () ((_ (x) g0 g ...) (run 31 (x) g0 g ...))))
(define-syntax run32 (syntax-rules () ((_ (x) g0 g ...) (run 32 (x) g0 g ...))))
(define-syntax run33 (syntax-rules () ((_ (x) g0 g ...) (run 33 (x) g0 g ...))))
(define-syntax run34 (syntax-rules () ((_ (x) g0 g ...) (run 34 (x) g0 g ...))))
(define-syntax run35 (syntax-rules () ((_ (x) g0 g ...) (run 35 (x) g0 g ...))))
(define-syntax run36 (syntax-rules () ((_ (x) g0 g ...) (run 36 (x) g0 g ...))))
(define-syntax run37 (syntax-rules () ((_ (x) g0 g ...) (run 37 (x) g0 g ...))))
(define-syntax run38 (syntax-rules () ((_ (x) g0 g ...) (run 38 (x) g0 g ...))))
(define-syntax run39 (syntax-rules () ((_ (x) g0 g ...) (run 39 (x) g0 g ...))))
(define-syntax run40 (syntax-rules () ((_ (x) g0 g ...) (run 40 (x) g0 g ...))))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (x)
    (== '() x)))

(define eqo
  (lambda (x y)
    (== x y)))

(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

(define membero
  (lambda (x l)
    (conde      
      ((fresh (a)
         (caro l a)
         (== a x)))
      ((fresh (d)
         (cdro l d)
         (membero x d))))))

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((caro l x) (cdro l out))
      ((fresh (a d res)
         (conso a d l)
         (rembero x d res)
         (conso a res out))))))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (conso a d l)
         (conso a res out)
         (appendo d s res))))))

(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== '() out))
      ((pairo s)
       (fresh (a d res-a res-d)
         (conso a d s)
         (flatteno a res-a)
         (flatteno d res-d)
         (appendo res-a res-d out)))
      ((conso s '() out)))))

(define anyo
  (lambda (g)
    (conde
      (g)
      ((anyo g)))))

(define nevero (anyo fail))
(define alwayso (anyo succeed))

(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))    
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((zero? n) '()))))

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(define >1o
  (lambda (n)
    (fresh (a ad dd)
      (== `(,a ,ad . ,dd) n))))

(define full-addero
  (lambda (b x y r c)
    (conde
      ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c)))))

(define addero
  (lambda (d n m r)
    (conde
      ((== 0 d) (== '() m) (== n r))
      ((== 0 d) (== '() n) (== m r)
       (poso m))
      ((== 1 d) (== '() m)
       (addero 0 n '(1) r))
      ((== 1 d) (== '() n) (poso m)
       (addero 0 '(1) m r))
      ((== '(1) n) (== '(1) m)
       (fresh (a c)
         (== `(,a ,c) r)
         (full-addero d 1 1 a c)))
      ((== '(1) n) (gen-addero d n m r))
      ((== '(1) m) (>1o n) (>1o r)
       (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r)))))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (full-addero d a b c e)
      (addero e x y z))))

(define pluso
  (lambda (n m k)
    (addero 0 n m k)))

(define minuso
  (lambda (n m k)
    (pluso m k n)))

(define *o
  (lambda (n m p)
    (conde
      ((== '() n) (== '() p))
      ((poso n) (== '() m) (== '() p))  
      ((== '(1) n) (poso m) (== m p))   
      ((>1o n) (== '(1) m) (== n p))
      ((fresh (x z)
         (== `(0 . ,x) n) (poso x)
         (== `(0 . ,z) p) (poso z)
         (>1o m)
         (*o x m z)))
      ((fresh (x y)
         (== `(1 . ,x) n) (poso x)
         (== `(0 . ,y) m) (poso y)
         (*o m n p)))
      ((fresh (x y)
         (== `(1 . ,x) n) (poso x)      
         (== `(1 . ,y) m) (poso y)
         (odd-*o x n m p))))))

(define odd-*o
  (lambda (x n m p)
    (fresh (q)
      (bound-*o q p n m)
      (*o x m q)
      (pluso `(0 . ,q) m p))))

(define bound-*o
  (lambda (q p n m)
    (conde
      ((nullo q) (pairo p))
      ((fresh (x y z)
         (cdro q x)
         (cdro p y)
         (conde
           ((nullo n)
            (cdro m z)
            (bound-*o x y z '()))
           ((cdro n z) 
            (bound-*o x y z m))))))))

(define =lo
  (lambda (n m)
    (conde
      ((== '() n) (== '() m))
      ((== '(1) n) (== '(1) m))
      ((fresh (a x b y)
         (== `(,a . ,x) n) (poso x)
         (== `(,b . ,y) m) (poso y)
         (=lo x y))))))

(define <lo
  (lambda (n m)
    (conde
      ((== '() n) (poso m))
      ((== '(1) n) (>1o m))
      ((fresh (a x b y)
         (== `(,a . ,x) n) (poso x)
         (== `(,b . ,y) m) (poso y)
         (<lo x y))))))

(define <=lo
  (lambda (n m)
    (conde
      ((=lo n m))
      ((<lo n m)))))

(define <o
  (lambda (n m)
    (conde
      ((<lo n m))
      ((=lo n m)
       (fresh (x)
         (poso x)
         (pluso n x m))))))

(define <=o
  (lambda (n m)
    (conde
      ((== n m))
      ((<o n m)))))

(define /o
  (lambda (n m q r)
    (conde
      ((== r n) (== '() q) (<o n m))
      ((== '(1) q) (=lo n m) (pluso r m n)
       (<o r m))
      ((<lo m n)                        
       (<o r m)                        
       (poso q)                 
       (fresh (nh nl qh ql qlm qlmr rr rh)
         (splito n r nl nh)
         (splito q r ql qh)
         (conde
           ((== '() nh)
            (== '() qh)
            (minuso nl r qlm)
            (*o ql m qlm))
           ((poso nh)
            (*o ql m qlm)
            (pluso qlm r qlmr)
            (minuso qlmr nl rr)
            (splito rr r '() rh)
            (/o nh m qh rh))))))))

(define splito
  (lambda (n r l h)
    (conde
      ((== '() n) (== '() h) (== '() l))
      ((fresh (b n^)
         (== `(0 ,b . ,n^) n)
         (== '() r)
         (== `(,b . ,n^) h)
         (== '() l)))
      ((fresh (n^)
         (==  `(1 . ,n^) n)
         (== '() r)
         (== n^ h)
         (== '(1) l)))
      ((fresh (b n^ a r^)
         (== `(0 ,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== '() l)
         (splito `(,b . ,n^) r^ '() h)))
      ((fresh (n^ a r^)
         (== `(1 . ,n^) n)
         (== `(,a . ,r^) r)
         (== '(1) l)
         (splito n^ r^ '() h)))
      ((fresh (b n^ a r^ l^)
         (== `(,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== `(,b . ,l^) l)
         (poso l^)
         (splito n^ r^ l^ h))))))

(define logo
 (lambda (n b q r)
   (conde
     ((== '(1) n) (poso b) (== '() q) (== '() r))
     ((== '() q) (<o n b) (pluso r '(1) n))
     ((== '(1) q) (>1o b) (=lo n b) (pluso r b n))
     ((== '(1) b) (poso q) (pluso r '(1) n))
     ((== '() b) (poso q) (== r n))
     ((== '(0 1) b)
      (fresh (a ad dd)
        (poso dd)
        (== `(,a ,ad . ,dd) n)
        (exp2 n '() q)
        (fresh (s)
          (splito n dd r s))))
     ((fresh (a ad add ddd)
        (conde
          ((== '(1 1) b))
          ((== `(,a ,ad ,add . ,ddd) b))))
      (<lo b n)
      (fresh (bw1 bw nw nw1 ql1 ql s)
        (exp2 b '() bw1)
        (pluso bw1 '(1) bw)
        (<lo q n)
        (fresh (q1 bwq1)
          (pluso q '(1) q1)
          (*o bw q1 bwq1)
          (<o nw1 bwq1))
          (exp2 n '() nw1)
          (pluso nw1 '(1) nw)
          (/o nw bw ql1 s)
          (pluso ql '(1) ql1)
          (<=lo ql q)
          (fresh (bql qh s qdh qd)
            (repeated-mul b ql bql)
            (/o nw bw1 qh s)
            (pluso ql qdh qh)
            (pluso ql qd q)
            (<=o qd qdh)
            (fresh (bqd bq1 bq)
              (repeated-mul b qd bqd)
              (*o bql bqd bq)
              (*o b bq bq1)
              (pluso bq r n)
              (<o n bq1))))))))

(define exp2
  (lambda (n b q)
    (conde
      ((== '(1) n) (== '() q))
      ((>1o n) (== '(1) q)
       (fresh (s)
         (splito n b s '(1))))
      ((fresh (q1 b2)
         (== `(0 . ,q1) q)
         (poso q1)
         (<lo b n)
         (appendo b `(1 . ,b) b2)
         (exp2 n b2 q1)))
      ((fresh (q1 nh b2 s)
         (== `(1 . ,q1) q)
         (poso q1)
         (poso nh)
         (splito n b s nh)
         (appendo b `(1 . ,b) b2)
         (exp2 nh b2 q1))))))

(define repeated-mul
  (lambda (n q nq)
    (conde
      ((poso n) (== '() q) (== '(1) nq))
      ((== '(1) q) (== n nq))
      ((>1o q)
       (fresh (q1 nq1)
         (pluso q1 '(1) q)
         (repeated-mul n q1 nq1)
         (*o nq1 n nq))))))

(define expo
  (lambda (b q n)
    (logo n b q '())))

(define prnt
  (lambda (vars)
    (lambda (expr)
      (lambda (s)
        (begin
          (write expr)
          (newline)
          (write (map (lambda (p)
                        `(,(car p) ,(walk* (cdr p) s)))
                      vars))
          (newline)
          (succeed s))))))

