(in-package :icfp2014)

(defvar *appendices*)

(define-constant +constants+
  '((wall . 0) (empty . 1) (pill . 2) (power-pill . 3)
    (fruit-pos . 4) (lambda-man-start . 5) (ghost-start . 6)
    (standard . 0) (fright-mode . 1) (invisible . 2)
    (up . 0) (right . 1) (down . 2) (left . 3))
  :test 'equal)

;;; Instructions

(defmacro definstr (name str)
  (let ((sym (intern (format nil "GCC-~a" name))))
    `(defun ,sym (&rest args)
       ,str
       (format t "~:@(~a~)~{~^ ~a~}~%" ',name args))))

(definstr ldc "Load constant")
(definstr ld "Load from environment")
(definstr add "Integer addition")
(definstr sub "Integer subtraction")
(definstr mul "Integer multiplication")
(definstr div "Integer division")
(definstr ceq "Compare equal")
(definstr cgt "Compare greater than")
(definstr cgte "Compare greater than or equal")
(definstr atom "Test if value is an integer")
(definstr cons "Allocate a CONS cell")
(definstr car "Extract first element from CONS cell")
(definstr cdr "Extract second element from CONS cell")
(definstr sel "Conditional branch")
(definstr join "Return from branch")
(definstr ldf "Load function")
(definstr ap "Call function")
(definstr rtn "Return from function call")
(definstr dum "Create empty environment frame")
(definstr rap "Recursive environment call function")
(definstr stop "Terminate co-processor execution [use RTN instead]")
(definstr tsel "Tail-call conditional branch")
(definstr tap "Tail-call function")
(definstr trap "Recursive environment tail-call function")
(definstr st "Store to environment")
(definstr dbug "Printf debugging")
(definstr brk "Breakpoint debugging")


;;; Transformators

(defun gcc-fn (expr env &rest body)
  `(,@(iter (for i from 1 below (length expr))
            (appending (gcc-transform (nth i expr) env)))
    ,@body))

(defun gcc-seq (exprs env)
  (iter (for subexpr in exprs)
        (appending (gcc-transform subexpr env))))

(defun gcc-lookup (symbol env)
  (iter (for frame in env)
        (for i upfrom 0)
        (iter (for var in frame)
              (for j upfrom 0)
              (when (eq symbol var)
                (return-from gcc-lookup `((gcc-ld ,i ,j))))))
  `((gcc-ldf ,symbol)))

(defun gcc-number-or-constant (expr env)
  (let ((str (format nil "~a" expr)))
    (if (char= (char str 0) (char str (1- (length str))) #\+)
        (let* ((sym (intern (subseq str 1 (1- (length str)))))
               (found (assoc sym +constants+)))
          (if found
              `((gcc-ldc ,(cdr found)))
              (error "Unknown constant: ~a" str)))
        (gcc-lookup expr env))))

(defun gcc-let (expr env)
  (let* ((n (length (second expr)))
         (vars (iter (for (var . rest) in (second expr))
                     (collect (gensym (format nil "~a" var)))))
         (let-symbol (gensym "LET"))
         (new-env (cons (mapcar #'first (second expr)) env)))
    (push `((gcc-label ,let-symbol)
            ,@(gcc-seq (nthcdr 2 expr) new-env)
            (gcc-rtn)
            ,@(iter (for letexpr in (second expr))
                    (for var in vars)
                    (when (> (length letexpr) 2)
                      (appending (gcc-transform `(def ,var ,@(rest letexpr)) new-env)))))
          *appendices*)
    `((gcc-dum ,n)
      ,@(iter (for letexpr in (second expr))
              (for var in vars)
              (if (= (length letexpr) 2)
                  ;; variable
                  (appending (gcc-transform (second letexpr) new-env))
                  ;; function
                  (appending (gcc-lookup var env))))
      (gcc-ldf ,let-symbol)
      (gcc-rap ,n))))

(defun gcc-transform (expr env)
  (cond ((numberp expr)
         `((gcc-ldc ,expr)))
        ((atom expr)
         (gcc-number-or-constant expr env))
        (t (case (first expr)
             (return '((gcc-rtn)))
             (def (let ((new-env (cons (third expr) env)))
                    `((gcc-label ,(second expr))
                      ,@(gcc-seq (nthcdr 3 expr) new-env)
                      (gcc-rtn))))
             (let (gcc-let expr env))
             (cons (gcc-fn expr env '(gcc-cons)))
             (+ (gcc-fn expr env '(gcc-add)))
             (- (gcc-fn expr env '(gcc-sub)))
             (* (gcc-fn expr env '(gcc-mul)))
             (/ (gcc-fn expr env '(gcc-div)))
             (t `(,@(gcc-seq (rest expr) env)
                  ,@(gcc-lookup (first expr) env)
                  (gcc-ap ,(1- (length expr)))))))))

(defmacro gcc-postprocess (&body exprs)
  "TODO: Uses the gensyms' textual representation as a key,
as genysms are not EQ to themselves..."
  (let ((addrs (make-hash-table :test 'equal)))
    (iter (for i first 0 then (if labelp i (1+ i)))
          (for expr in exprs)
          (for labelp = (eq (first expr) 'gcc-label))
          (when labelp
            (setf (gethash (format nil "~a" (second expr)) addrs) i)))
    `(progn
       ,@(iter (for expr in exprs)
               (cond ((eq (first expr) 'gcc-label) nil)
                     ((eq (first expr) 'gcc-ldf)
                      (let ((line (gethash (format nil "~a" (second expr)) addrs)))
                        (if line
                            (collect `(gcc-ldf ,line))
                            (error "Unknown label: ~a" (second expr)))))
                     (t (collect expr)))))))


;;; Top-level macro

(defmacro gcc (&body body)
  (setf *appendices* '())
  `(gcc-postprocess
     ,@(iter (for expr in body)
             (appending (gcc-transform expr '(()))))
     ,@(reduce #'append *appendices*)))
