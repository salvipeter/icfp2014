(in-package :icfp2014)

;;; Emacs macro for copying the whole code:
;; (fset 'ghost [?\M-> ?\C-x ?\C-e ?\C-x ?\C-x ?\C-x ?\C-f ?/ ?t ?m ?p ?/ ?g ?h ?o ?s ?t ?0 ?. ?g ?h ?c return ?\C-x ?h ?\M-w ?\C-x ?k return])

;;; Ideas:
;;; - all of them should flee in fright mode
;;; - all of them should chase lambdaman if he comes into view (n moves away)
;;; - all of them should avoid other ghosts
;;; - one guards power pills
;;; - one guards the fruit position
;;; - two wander aimlessly, selecting a good direction visited last

;;; Notes:
;;; C is the loop counter
;;; [000]-[003] store the scores for directions (a score of 0 means we cannot go there)
;;; [004]-[005] our coordinates
;;; [006]-[007] lambdaman's coordinates
;;; [010]-[017] candidate coordinates
;;; [100]-[199] store the last visited positions
;;;             (x in the even, y in the odd addresses)
;;; [200]       stores the address of the last visited position
;;;             (e.g. 118, the next one will be saved in 120, etc.)
;;; [255]       reserved (always 0)

(with-open-file (s "/tmp/ghost0.ghc" :direction :output :if-exists :supersede)
  (let ((*standard-output* s))
    (ghc

;;; GHC code starts

; Set [200] to 100 if this is the first time to run
(when= [200] 0
  (mov [200] 100))

; Get Lambda-Man's position
(lman1-pos)
(mov [6] a)
(mov [7] b)

; Save our position
(get-index)
(ghost-pos)
(mov [4] a)
(mov [5] b)

; Compute candidate coordinates
(mov [10] a)
(mov [11] b)
(dec [11])
(mov [12] a)
(inc [12])
(mov [13] b)
(mov [14] a)
(mov [15] b)
(inc [15])
(mov [16] a)
(dec [16])
(mov [17] b)

;;; Evaluate the candidates
(mov d 0) ; candidate index
(for 10 18
  (mov a [c])
  (inc c)
  (mov b [c])
  ;; check whether this is where lambdaman is
  (when= a [6]
    (when= b [7]
       (mov [d] 255)
       (jmp _next)))
  (get-cell)
  (if= a +wall+
    ((mov [d] 0))
    ((if= a +pill+
       ((mov [d] 5))
       ((if= a +power-pill+
          ((mov [d] 10))
          ((mov [d] 1)))))))
_next
  (inc d))

;;; Set the reverse direction's score to 0
(get-index)
(ghost-direction)
(if> b 1
  ((sub b 2))
  ((add b 2)))
(mov [b] 0)

;;; Select the best direction
(mov a 255)
(for 0 4
  (when> [c] [a]
    (mov a c)))
(when= a 255
  (mov a 0)) ; no good move
(set-direction)

;;; GHC code ends

)))
