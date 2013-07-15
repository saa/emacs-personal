(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(set-default-font "Menlo-13")

;; HOOKS
(defun disable-flyspell-mode ()
  (flyspell-mode -1))

(setq prelude-guru nil)
(setq prelude-flyspell nil)

(remove-hook 'mouse-leave-buffer-hook #'prelude-auto-save-command)
(add-hook 'prelude-prog-mode-hook 'disable-flyspell-mode t)
(remove-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prelude-coding-hook 'flyspell-prog-mode)
(remove-hook 'text-mode-hook 'prelude-turn-on-flyspell)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)

;; DEFUNS
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
A place is considered 1 character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) 1))))

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))

(defun run-prog-hook ()
  (interactive)
  (run-hooks 'prog-hook))

(defun copy-all ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;; REQUIRES
(require 'smex)
(smex-initialize)

(require 'ace-jump-mode)

;; BINDINGS
(prelude-swap-meta-and-super)
(global-set-key (kbd "C-0") 'text-scale-normal-size)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-z") nil)

(global-set-key (kbd "C->") 'textmate-shift-right)
(global-set-key (kbd "C-<") 'textmate-shift-left)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)

(define-key global-map (kbd "C-`") 'ace-jump-word-mode)

(global-set-key (kbd "C-C C-SPC") 'copy-all)

(global-set-key (kbd "C-c C-b") 'comment-dwim)
(global-set-key (kbd "C-c k") 'delete-region)
(global-set-key (kbd "C-c C-f") 'ns-toggle-fullscreen)

(global-set-key (kbd "C-c ]") 'comment-dwim)

;; SETTINGS

(setq-default tab-width 4)
(global-rainbow-delimiters-mode 1)

(mouse-avoidance-mode 'cat-and-mouse)

(setq
 cursor-in-non-selected-windows nil
 use-dialog-box nil
 whitespace-line-column 80
 whitespace-style '(face
                    trailing
                    lines
                    tabs
                    space-before-tab space-after-tab
                    indentation))

(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p)

(pending-delete-mode t)

(require 'paren)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "#6f6f6f")
(set-face-foreground 'show-paren-match-face "#94bff3")

(global-auto-revert-mode 1)

;; Auto-complete
(require 'auto-complete nil t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config nil t)
(ac-config-default)
(setq ac-compish-file "~/.emacs.d/ac-comphist.dat"
      ac-candidate-limit 20
      ac-ignore-case nil)
(global-auto-complete-mode)

;; Python
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:setup-keys t)

;; Tramp
(setq tramp-default-method "ssh")

;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; erlang
;; (require 'erlang-start)

;; (add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
;; (add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

;; (require 'erlang-flymake)

;; (setq erlang-root-dir "/usr/local/Cellar/erlang/R15B03-1/lib/erlang")
;; (setq erlang-man-root-dir "/usr/local/Cellar/erlang/R15B03-1/share/man")
;; (add-to-list 'load-path
;;              (concat erlang-root-dir "/lib/tools-2.6.8/emacs"))
;; (add-to-list 'exec-path
;;              (concat erlang-root-dir "/bin"))
;; (add-to-list 'ac-modes 'erlang-mode)

;; (defun my-erlang-mod-hook ()
;;   (setq inferior-erlang-machine-options '("-sname" "emacs"))
;;   (imenu-add-to-menubar "imenu")
;;   (local-set-key [return] 'newline-and-indent))

;; (add-hook 'erlang-mode-hook 'my-erlang-mod-hook)

;; (add-to-list 'load-path "~/.emacs.d/vendor/distel/elisp")
;; (require 'distel)
;; (distel-setup)


;; Jabber
;; (add-to-list 'load-path "~/.emacs.d/elpa/emacs-jabber-0.8.92")
;; (require 'jabber-autoloads)
