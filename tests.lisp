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
