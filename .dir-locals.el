;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((lisp-mode . ((outshine-startup-folded-p . t)
	       (comment-empty-lines . t)
	       (comment-style . 'plain)
	       (outline-regexp . ";;; [*]\\{1,8\\} ")
	       (comment-add . 2)
	       (eval . (progn
			 (outshine-mode 1)
			 (column-enforce-mode 1)
			 (toggle-truncate-lines 1)))
	       (fill-column . 95)
	       (column-enforce-column . 110))))

;; Note: outshine-startup-folded-p does not work (even as global
;; variable), perhaps because outshine mode is activated too late
;; (after file loading). I'll have to look for the recommendations in
;; the outshine install instructions and see if this is going to give
;; more joy in this regard.
