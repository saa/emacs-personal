(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "personal/modes/main.el")
(load-user-file "personal/modes/hooks.el")
(load-user-file "personal/modes/adv.el")
(load-user-file "personal/modes/erlang.el")
(load-user-file "personal/modes/ocaml.el")
(load-user-file "personal/modes/functions.el")
(load-user-file "personal/modes/bindings.el")
