(in-package :icfp2014)

;;; Emacs macro for copying the whole code:
;; (fset 'ghost [?\M-> ?\C-x ?\C-e ?\C-x ?\C-x ?\C-x ?\C-f ?/ ?t ?m ?p ?/ ?g ?h ?o ?s ?t ?0 ?. ?g ?h ?c return ?\C-x ?h ?\M-w ?\C-x ?k return])

(with-open-file (s "/tmp/ghost0.ghc" :direction :output :if-exists :supersede)
  (let ((*standard-output* s))
    (ghc

;;; GHC code starts

(get-index)
(ghost-pos)
(and a 1)
(mov b a)
(mov a +down+)
(jeq _odd b 1)
(mov a +up+)

_odd
(set-direction)

;;; GHC code ends

      )))
