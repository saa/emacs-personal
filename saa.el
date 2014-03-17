(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init--directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "personal/modes/main.el")
(load-user-file "personal/modes/functions.el")
(load-user-file "personal/modes/ocaml.el")

;; Hooks
(remove-hook 'mouse-leave-buffer-hook #'prelude-auto-save-command)
(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c '") 'comment-dwim)
(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "s-<up>") 'move-text-up)
(global-set-key (kbd "s-<down>") 'move-text-down)
(global-set-key (kbd "C-0") 'text-scale-normal-size)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Erlang
(add-hook 'erlang-mode-hook (load-user-file "personal/modes/erlang.el"))
(add-hook 'erlang-mode-hook (lambda () (autopair-mode)))
(add-hook 'erlang-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
