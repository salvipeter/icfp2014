(in-package :icfp2014)

(defvar *labels*)
(defvar *counter*)
(defvar *pass*)

(define-constant +wall+ 0)
(define-constant +empty+ 1)
(define-constant +pill+ 2)
(define-constant +power-pill+ 3)
(define-constant +fruit-pos+ 4)
(define-constant +lambda-man-start+ 5)
(define-constant +ghost-start+ 6)
(define-constant +standard+ 0)
(define-constant +fright-mode+ 1)
(define-constant +invisible+ 2)
(define-constant +up+ 0)
(define-constant +right+ 1)
(define-constant +down+ 2)
(define-constant +left+ 3)

;;; Assembler instructions

(defmacro defasm (name)
  (let ((sym (intern (format nil "GHC-~a" name))))
    `(defmacro ,sym (&rest args)
       (let ((args (iter (for arg in args)
                         (for str = (format nil "~a" arg))
                         (cond ((char= (char str 0) (char str (1- (length str))) #\+)
                                (collect arg))
                               ((and (char= (char str 0) #\[) (char= (char str 1) #\+)
                                     (char= (char str (- (length str) 2)) #\+))
                                (collect `(format nil "[~a]"
                                                  ,(intern (subseq str 1 (1- (length str)))))))
                               ((and (char= (char str 0) #\_))
                                (collect `(ghc-lookup ',arg)))
                               (t (collect (list 'quote arg)))))))
         `,`(if (= *pass* 1)
                (incf *counter*)
                (format t "~:@(~a~)~{~^ ~a~^,~}~%" ',',name ,`(list ,@args)))))))

(defasm mov)
(defasm inc)
(defasm dec)
(defasm add)
(defasm sub)
(defasm mul)
(defasm div)
(defasm and)
(defasm or)
(defasm xor)
(defasm jlt)
(defasm jeq)
(defasm jgt)
(defasm int)
(defasm hlt)

(defun ghc-label (name)
  (when (= *pass* 1)
    (setf (gethash (format nil "~a" name) *labels*) *counter*)))

(defun ghc-lookup (name)
  (let ((line (gethash (format nil "~a" name) *labels*)))
    (or line (error "Unknown label: ~a" name))))


;;; Interrupts

(defmacro definterrupt (name n)
  (let ((sym (intern (format nil "GHC-~a" name))))
    `(defmacro ,sym ()
       ',`(ghc-int ,n))))

(definterrupt set-direction   0)
(definterrupt lman1-pos       1)
(definterrupt lman2-pos       2)
(definterrupt get-index       3)
(definterrupt ghost-start     4)
(definterrupt ghost-pos       5)
(definterrupt ghost-vitality  6)
(definterrupt ghost-direction 6)
(definterrupt get-cell        7)
(definterrupt debug           8)


;;; High-level programming constructs

(defmacro defconstr (name args &body body)
  `(defmacro ,(intern (format nil "GHC-~a" name)) ,args
     `,`(progn ,@(mapcar #'prepend-ghc `,,@body))))

(defconstr seq (&body body)
  body)

(defconstr jmp (where)
  `((jeq ,where 0 0)))

(defconstr if= (a b if-true if-false)
  (let ((true (gensym "_TRUE"))
        (endif (gensym "_ENDIF")))
    `((jeq ,true ,a ,b)
      ,@if-false
      (jmp ,endif)
      ,true
      ,@if-true
      ,endif)))

(defconstr if< (a b if-true if-false)
  (let ((true (gensym "_TRUE"))
        (endif (gensym "_ENDIF")))
    `((jlt ,true ,a ,b)
      ,@if-false
      (jmp ,endif)
      ,true
      ,@if-true
      ,endif)))

(defconstr if> (a b if-true if-false)
  `((if< ,b ,a ,if-true ,if-false)))

(defconstr if<= (a b if-true if-false)
  `((if> ,a ,b ,if-false ,if-true)))

(defconstr if>= (a b if-true if-false)
  `((if< ,a ,b ,if-false ,if-true)))

(defconstr unless= (a b &body body)
  (let ((true (gensym "_TRUE")))
    `((jeq ,true ,a ,b)
      ,@body
      ,true)))

(defconstr when<= (a b &body body)
  (let ((greater (gensym "_GREATER")))
    `((jgt ,greater ,a ,b)
      ,@body
      ,greater)))

(defconstr when>= (a b &body body)
  (let ((less (gensym "_LESS")))
    `((jlt ,less ,a ,b)
      ,@body
      ,less)))

(defconstr when= (a b &body body)
  `((if= ,a ,b ,body ())))

(defconstr when< (a b &body body)
  `((if< ,a ,b ,body ())))

(defconstr when> (a b &body body)
  `((if> ,a ,b ,body ())))

(defconstr for (from below &body body)
  (let ((label (gensym "_LOOP")))
    `((mov c ,from)
      ,label
      ,@body
      (inc c)
      (jlt ,label c ,below))))


;;; Top-level macro

(defun prepend-ghc (expr)
  (if (atom expr)
      `(ghc-label ',expr)
      `(,(intern (format nil "GHC-~a" (first expr))) ,@(rest expr))))

(defun expand (expr)
  (cond ((atom expr) expr)
        ((eq (first expr) 'progn)
         `(progn ,@(iter (for subexpr in (rest expr))
                         (collect (expand subexpr)))))
        (t (multiple-value-bind (value expandedp)
               (macroexpand expr)
             (if expandedp
                 (expand value)
                 value)))))

(defmacro ghc (&body body)
  (setf *counter* 0)
  (setf *labels* (make-hash-table :test 'equal))
  (let ((complete-body `(progn ,(expand (cons 'progn (mapcar #'prepend-ghc body))) (ghc-hlt))))
    `(progn
       (let ((*pass* 1))
         ,complete-body)
       (let ((*pass* 2))
         ,complete-body))))
