(in-package :icfp2014)

;;; Emacs macro for copying the whole code:
;; (fset 'lambdaman [?\M-> ?\C-x ?\C-e ?\C-x ?\C-x ?\C-x ?\C-f ?/ ?t ?m ?p ?/ ?l ?a ?m ?b ?d ?a ?m ?a ?n ?. ?g ?c ?c return ?\C-x ?h ?\M-w ?\C-x ?k return])

(with-open-file (s "/tmp/lambdaman.gcc" :direction :output :if-exists :supersede)
  (let ((*standard-output* s))
    (gcc

;;; GCC code starts

(cons 0 step)
(return)

(def step (back-dir world)
  (let* ((map (car world))
         (lman (cadr world))
         (ghosts (caddr world))
         (fruit (cdddr world))
         (xy (cadr lman))
         (dirs (cons +left+ (cons +right+ (cons +up+ (cons +down+ 0)))))
         (candidates (mapcar (lambda (dir) (cons dir (cell map (movement xy dir)))) dirs))
         (good-dirs (remove-if-not (lambda (v)
                                     (and (> (cdr v) +wall+)
                                          (/= (car v) back-dir)))
                                   candidates))
         (next-step (car (reduce (lambda (a b)
                                   (if (and (> (cdr a) (cdr b))
                                            (< (cdr a) +lambda-man-start+))
                                       a
                                       b))
                                 good-dirs))))
    (cons (reverse-dir next-step) next-step)))

(def cell (map xy)
  (nth (car xy) (nth (cdr xy) map)))

(def reverse-dir (dir)
  (if (> dir 1)
      (- dir 2)
      (+ dir 2)))
(def left (xy)
  (cons (- (car xy) 1) (cdr xy)))
(def right (xy)
  (cons (+ (car xy) 1) (cdr xy)))
(def up (xy)
  (cons (car xy) (- (cdr xy) 1)))
(def down (xy)
  (cons (car xy) (+ (cdr xy) 1)))
(def movement (xy dir)
  (if (= dir +left+)
      (left xy)
      (if (= dir +right+)
          (right xy)
          (if (= dir +up+)
              (up xy)
              (down xy)))))

(def < (a b)
  (> b a))
(def <= (a b)
  (>= b a))
(def /= (a b)
  (not (= a b)))

(def and (a b)
  (* a b))
(def not (a)
  (- 1 a))
(def or (a b)
  (not (and (not a) (not b))))

(def cadr (lst)
  (car (cdr lst)))
(def cddr (lst)
  (cdr (cdr lst)))
(def caddr (lst)
  (car (cdr (cdr lst))))
(def cdddr (lst)
  (cdr (cdr (cdr lst))))

(def nth (n lst)
  (if (= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))
(def mapcar (fn lst)
  (if (atom lst)
      lst
      (cons (fn (car lst)) (mapcar fn (cdr lst)))))
(def reduce (fn lst)
  (if (atom (cdr lst))
      (car lst)
      (reduce fn (cons (fn (car lst) (cadr lst)) (cddr lst)))))
(def remove-if-not (pred lst)
  (if (atom lst)
      lst
      (if (pred (car lst))
          (cons (car lst) (remove-if-not pred (cdr lst)))
          (remove-if-not pred (cdr lst)))))

;;; GCC code ends

)))
