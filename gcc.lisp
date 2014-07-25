(in-package :icfp2014)

(defvar *appendices*)

(define-constant +constants+
  '((wall . 0) (empty . 1) (pill . 2) (power-pill . 3)
    (fruit-pos . 4) (lambda-man-start . 5) (ghost-start . 6)
    (standard . 0) (fright-mode . 1) (invisible . 2)
    (up . 0) (right . 1) (down . 2) (left . 3))
  :test 'equal)

;;; Instructions
;;; LDC  - load constant
;;; LD   - load from environment
;;; ADD  - integer addition
;;; SUB  - integer subtraction
;;; MUL  - integer multiplication
;;; DIV  - integer division
;;; CEQ  - compare equal
;;; CGT  - compare greater than
;;; CGTE - compare greater than or equal
;;; ATOM - test if value is an integer
;;; CONS - allocate a CONS cell
;;; CAR  - extract first element from CONS cell
;;; CDR  - extract second element from CONS cell
;;; SEL  - conditional branch
;;; JOIN - return from branch
;;; LDF  - load function
;;; AP   - call function
;;; RTN  - return from function call
;;; DUM  - create empty environment frame
;;; RAP  - recursive environment call function
;;; STOP - terminate co-processor execution [use RTN instead]
;;; TSEL - tail-call conditional branch
;;; TAP  - tail-call function
;;; TRAP - recursive environment tail-call function
;;; ST   - store to environment
;;; DBUG - printf debugging
;;; BRK  - breakpoint debugging


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
  `((gcc-ldf ',symbol)))

(defun gcc-atom (expr env)
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
      (gcc-ldf ',let-symbol)
      (gcc-rap ,n))))

(defun gcc-transform (expr env)
  (cond ((numberp expr)
         `((gcc-ldc ,expr)))
        ((atom expr)
         (gcc-atom expr env))
        (t (case (first expr)
             (return '((gcc-rtn)))
             (def (let ((new-env (cons (third expr) env)))
                    `((gcc-label ',(second expr))
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


;;; Top-level macro

(defmacro gcc (&body body)
  (setf *appendices* '())
  `(progn
     ,@(iter (for expr in body)
             (appending (gcc-transform expr '(()))))
     ,@(reduce #'append *appendices*)))
