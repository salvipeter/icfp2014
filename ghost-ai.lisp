(in-package :icfp2014)

;;; Emacs macro for copying the whole code:
;; (fset 'ghost [?\M-> ?\C-x ?\C-e ?\C-x ?\C-x ?\C-x ?\C-f ?/ ?t ?m ?p ?/ ?g ?h ?o ?s ?t ?0 ?. ?g ?h ?c return ?\C-x ?h ?\M-w ?\C-x ?k return])

;;; Ideas:
;;; - all of them should chase lambdaman if he comes into view (e.g. n moves away)
;;;   (or flee in fright mode)
;;; - ghost0 (x2) wanders aimlessly, selecting a good direction visited last
;;; - ghost1 guards power pills
;;; - ghost2 guards the fruit position

;;; Notes:
;;; C is the loop counter
;;; [000]-[003] store the scores for directions (a score of 0 means we cannot go there)
;;; [004]-[005] our coordinates
;;; [006]-[007] lambdaman's coordinates
;;; [010]-[017] candidate coordinates
;;; [020]-[027] ghosts' coordinates
;;; [100]-[199] store the last visited positions
;;;             (x in the even, y in the odd addresses)
;;; [200]       stores the address of the last visited position
;;;             (e.g. 118[-119], the next one will be saved in 116[-117], etc.)
;;; [255]       reserved (always 0)

(with-open-file (s "/tmp/ghost0.ghc" :direction :output :if-exists :supersede)
  (let ((*standard-output* s))
    (ghc

;;; GHC code starts

; Set [200] to 100 if this is the first time to run
(when= [200] 0
  (mov [200] 198))

; Get Lambda-Man's position
(lman1-pos)
(mov [6] a)
(mov [7] b)

; Save all ghost positions
(mov d 20)
(for 0 4
  (mov a c)
  (ghost-pos)
  (mov [d] a)
  (inc d)
  (mov [d] b)
  (inc d))

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

; Save the current position as visited
; and update the last visited pointer
(mov d [200])
(mov [d] a)
(inc d)
(mov [d] b)
(sub d 3)
(when= d 98
  (mov d 198))
(mov [200] d)

; Evaluate the candidates
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

; Set the reverse direction's score to 0
(get-index)
(ghost-direction)
(if> b 1
  ((sub b 2))
  ((add b 2)))
(mov [b] 0)

; For those directions still in play (score /= 0),
; modify the score based on the visited positions
(mov a 10)                              ; [A]: candidate position (CP)
(for 0 4
  (mov d c)                             ; [D]: candidate direction score
  (unless= [d] 0
    (mov b [200])                       ; [B]: visited position (VP)
    (add b 2)
    (for 0 10                           ; C: distance (visited length is set here)
      (when= b 200
        (mov b 100))
      (mov e [a])                       ; E: CPx
      (mov g [b])                       ; G: VPx
      (inc a)
      (inc b)
      (mov f [a])                       ; F: CPy
      (mov h [b])                       ; H: VPy
      (dec a)
      (inc b)
      (when= e g
        (when= f h
          (add [d] c)
          (jmp _visited))))
    (add [d] 50))
_visited
  (add a 2)
  (mov c d))

; Select the best direction
(mov a 255)
(for 0 4
  (when>= [c] [a]
    (mov a c)))
(set-direction)

;; (get-index)
;; (when= a 0
;;   (mov h [200])
;;   (mov a [h])
;;   (inc h)
;;   (mov b [h])
;;   (inc h)
;;   (mov c [h])
;;   (inc h)
;;   (mov d [h])
;;   (inc h)
;;   (mov e [h])
;;   (inc h)
;;   (mov f [h])
;;   (inc h)
;;   (mov g [h])
;;   (inc h)
;;   (mov h [h])
;;   (debug))

;;; GHC code ends

)))
