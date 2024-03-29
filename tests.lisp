;;; Sample programs

;;; GCC tests

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

;;; higher-order functions
(gcc (let ((max (x y) (if (> x y) x y)))
       (reduce max (cons 3 (cons 1 (cons 2 (cons 5 (cons 2 (cons 3 0))))))))
     (return)
     (def cadr (lst)
       (car (cdr lst)))
     (def cddr (lst)
       (cdr (cdr lst)))
     (def reduce (fn lst)
       (if (atom (cdr lst))
           (car lst)
           (reduce fn (cons (fn (car lst) (cadr lst)) (cddr lst))))))

;;; let* test
(gcc (let* ((a 1)
            (b (+ a 1)))
       b)
     (return))

;;; lambda test
(gcc (mapcar (lambda (x) (* x x)) (cons 1 (cons 2 (cons 3 (cons 4 0)))))
     (return)
     (def mapcar (fn lst)
       (if (atom lst)
           lst
           (cons (fn (car lst)) (mapcar fn (cdr lst))))))

;;; position test
(gcc (position (cons 1 2) (cons (cons 3 4) (cons (cons 5 6) (cons (cons 1 2) (cons (cons 7 8) 0))))
               pos=)
     (return)
     (def pos= (a b)
       (and (= (car a) (car b))
            (= (cdr a) (cdr b))))
     (def and (a b)
       (* a b))
     (def position (item seq pred)
       (let ((rec (lst n)
               (if (atom lst)
                   n
                   (if (pred (car lst) item)
                       n
                       (rec (cdr lst) (+ n 1))))))
         (rec seq 0))))

;;; take test
(gcc (take 3 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 0)))))))
     (return)
     (def take (n lst)
       (if (atom lst)
           0
           (if (= n 0)
               0
               (cons (car lst) (take (- n 1) (cdr lst)))))))


;;; GHC tests

;;; miner.ghc
(ghc (mov a +down+)
     (set-direction))

;;; flipper.ghc
(ghc (get-index)
     (ghost-pos)
     (and a 1)
     (mov b a)
     (mov a +down+)
     (unless= b 1
       (mov a +up+))
     (set-direction))

;;; flicker.ghc
(ghc (mov a 255)
     (mov b +up+)
     (for 0 4
       (when<= [c] a
         (mov a [c])
         (mov b c)))
     (mov a b)
     (set-direction)
     (get-index)
     (ghost-direction)
     (inc [b]))

;;; if= test
(ghc (mov a 1)
     (if= a 1
       ((mov a 2)
        (mov b 3))
       ((mov a 1)
        (mov b 2))))

;;; when> test
(ghc (mov a 1)
     (when> a 1
       (inc a))
     (mov b a))
