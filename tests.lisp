;;; Sample programs

;;; local.gcc
(gcc (body 21)
     (return)
     (def body (x)
       (+ x x)))

;;; goto.gcc
(gcc (let ((go (n) (to (+ n 1)))
           (to (n) (go (- n 1))))
       (go 1))
     (return))

;;; down.gcc
(gcc (let ((dir +down+)
           (step (s) (cons (+ s 1) dir)))
       (cons 42 step))
     (return))

;;; if.gcc
(gcc (if (> (+ 2 3) (- 6 1))
         1
         2)
     (return))
