(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(flyspell-mode-off)
(setq prelude-guru nil)
(setq prelude-flyspell nil)
(global-flycheck-mode -1)

(remove-hook 'mouse-leave-buffer-hook #'prelude-auto-save-command)
(remove-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prelude-coding-hook 'flyspell-prog-mode)
(remove-hook 'text-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(disable-theme 'zenburn)
(load-theme 'solarized-dark t)
(set-frame-font "Menlo-14")
(set-fontset-font "fontset-default" 'cyrillic '("menlo" . "ISO10646-1"))
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables '(shell-file-name "zsh"))
(ido-vertical-mode)
(setq-default tab-width 4)

(defun text-scale-normal-size ()
  (interactive)
  (text-scale-increase 0))

(defun textmate-shift-right (&optional arg)
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun textmate-shift-left (&optional arg)
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))

;; Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c '") 'comment-dwim)
(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "s-<up>") 'move-text-up)
(global-set-key (kbd "s-<down>") 'move-text-down)
(global-set-key (kbd "C-0") 'text-scale-normal-size)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)

(require 'smex)
(smex-initialize)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'ace-jump-mode)
(require 'auto-complete)
(require 'diminish)

(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(require 'midnight)

(require 'golden-ratio)
(golden-ratio-mode)
(diminish 'golden-ratio-mode)

(require 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)
(defalias 'redo 'undo-tree-redo)

(require 'paren)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "#6f6f6f")
(set-face-foreground 'show-paren-match-face "#94bff3")

(require 'whitespace)
(setq
 whitespace-line-column 120
 whitespace-style '(face
                    trailing
                    lines
                    lines-tail
                    tabs
                    space-before-tab space-after-tab
                    indentation))
(global-whitespace-mode t)

(require 'magit)
(setq magit-emacsclient-executable nil)

(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.13/emacs"
                       load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(setq erlang-indent-level 4)
(setq erlang-argument-indent 4)
(setq erlang-indent-guard 4)
(add-to-list 'ac-modes 'erlang-mode)
(add-hook 'erlang-mode-hook 'electric-pair-mode)
(add-hook 'erlang-mode-hook 'electric-indent-mode)
(add-to-list 'load-path "~/.emacs.d/vendor/edts")
(require 'edts-start)

(require 'protobuf-mode)

(require 'tuareg)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode))
          auto-mode-alist))
(cond
 ((eq window-system 'ns) ; macosx
  (setq shell-command-switch "-lc")))

(defun opam-vars ()
  (let* ((x (shell-command-to-string "opam config env"))
     (x (split-string x "\n"))
     (x (remove-if (lambda (x) (equal x "")) x))
     (x (mapcar (lambda (x) (split-string x ";")) x))
     (x (mapcar (lambda (x) (car x)) x))
     (x (mapcar (lambda (x) (split-string x "=")) x))
     )
    x))
(dolist (var (opam-vars))
  (setenv (car var) (substring (cadr var) 1 -1)))
(setq exec-path (split-string (getenv "PATH") path-separator))
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
              "/../../share/emacs/site-lisp") load-path)
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
(set-face-background 'merlin-type-face "#88FF44")

(require 'auto-complete)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
(load-file (expand-file-name "~/.emacs.d/vendor/ocp-indent.el"))

(add-hook 'clojure-mode-hook 'paredit-mode)
