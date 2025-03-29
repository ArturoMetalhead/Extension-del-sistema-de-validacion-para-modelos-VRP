;; key to insert (load everything.lisp) in the repl
(define-key lisp-mode-map (kbd "M-m M-o M-l")
  (lambda ()
    (interactive)
    (insert "(load \"src/load-lmml.lisp\") (in-package :lmml)")))

(define-key lisp-mode-map (kbd "M-m M-o M-o")
  (lambda ()
    (interactive)
    (insert "(turn-off-case-sensitivity)") (slime-repl-return)))

(define-key lisp-mode-map (kbd "M-m M-o M-i")
  (lambda ()
    (interactive)
    (insert "(turn-on-case-sensitivity)") (slime-repl-return)))

(define-key lisp-mode-map (kbd "M-m M-o M-s")
     (lambda ()
       (interactive)
       (insert "(check-case-sensitivity)") (slime-repl-return)))
