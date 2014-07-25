(in-package :icfp2014)

(define-constant +constants+
  '((wall . 0) (empty . 1) (pill . 2) (power-pill . 3)
    (fruit-pos . 4) (lambda-man-start . 5) (ghost-start . 6)
    (standard . 0) (fright-mode . 1) (invisible . 2)
    (up . 0) (right . 1) (down . 2) (left . 3)))

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

(defun gcc-fn (expr &rest body)
  `(,@(iter (for i from 1 below (length expr))
            (collect `(gcc-ld (progn ,@(gcc-transform (nth i expr))))))
    ,@body))

(defun gcc-seq (exprs)
  (iter (for subexpr in exprs)
        (appending (gcc-transform subexpr))))

(defun gcc-atom (expr)
  (let ((str (format nil "~a" expr)))
    (if (char= (char str 0) (char str (1- (length str))) #\+)
        (let* ((sym (intern (subseq str 1 (1- (length str)))))
               (found (assoc sym +constants+)))
          (if found
              `((gcc-ldc ,(cdr found)))
              (error "Unknown constant: ~a" str)))
        `((gcc-lookup ',expr)))))

(defun gcc-let (expr)
  (let ((n (length (second expr)))
        (vars (iter (for (var . rest) in (second expr))
                    (collect (gensym (format nil "~a" var)))))
        (let-symbol (gensym "LET")))
    `((gcc-dum ,n)
      ,@(iter (for letexpr in (second expr))
              (for var in vars)
              (if (= (length letexpr) 2)
                  ;; variable
                  (appending (gcc-transform (second letexpr)))
                  ;; function
                  (appending `((gcc-ldf (gcc-lookup ,var))))))
      (gcc-ldf (gcc-lookup ,let-symbol))
      (gcc-rap ,n)
      (gcc-def ,let-symbol ',(mapcar #'first (second expr)))
      ,@(gcc-seq (nthcdr 2 expr))
      ,@(iter (for letexpr in (second expr))
              (for var in vars)
              (when (> (length letexpr) 2)
                (appending (gcc-transform `(def ,var ,@(rest letexpr)))))))))

(defun gcc-transform (expr)
  (cond ((numberp expr)
         `((gcc-ldc ,expr)))
        ((atom expr)
         (gcc-atom expr))
        (t (case (first expr)
             (return '((gcc-rtn)))
             (def `((gcc-def ',(second expr) ',(third expr))
                    ,@(gcc-seq (nthcdr 3 expr))
                    (gcc-rtn)))
             (let (gcc-let expr))
             (+ (gcc-fn expr '(gcc-add)))
             (- (gcc-fn expr '(gcc-sub)))
             (* (gcc-fn expr '(gcc-mul)))
             (/ (gcc-fn expr '(gcc-div)))
             (t `(,@(gcc-seq (rest expr))
                  (gcc-ldf (progn ,@(gcc-transform (first expr))))
                  (gcc-ap ,(1- (length expr)))))))))


;;; Top-level macro

(defmacro gcc (&body body)
  `(progn
     ,@(iter (for expr in body)
             (appending (gcc-transform expr)))))
