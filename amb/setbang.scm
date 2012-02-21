> (amb (omega) (+ 5 (call/cc (lambda (k) (set! y k) 120))))
125
> (set! x (amb (omega) (+ 5 (call/cc (lambda (k) (set! y k) 120)))))
> (y x)
> x
130
> (y x)
> x
135
> (set! xx (+ 5 (call/cc (lambda (k) (set! yy k) 120))))
> xx
125
> (yy xx)
> xx
130
> (yy xx)
> xx
135
> (set! xxx (amb (omega) (amb (omega) (+ 5 (call/cc (lambda (k) (set! yyy k) 120
))))))
> xxx
125
> (yyy xxx)
> xxx
130
> (yyy xxx)
> xxx
135
> 
