(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(set-default-font "Menlo-13")

(flyspell-mode-off)
(setq prelude-guru nil)
(setq prelude-flyspell nil)

;; Functions
(defun disable-flyspell-mode ()
  (flyspell-mode -1))

;; Hooks
(remove-hook 'mouse-leave-buffer-hook #'prelude-auto-save-command)
(remove-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prelude-coding-hook 'flyspell-prog-mode)
(remove-hook 'text-mode-hook 'prelude-turn-on-flyspell)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'prog-mode-hook 'disable-flyspell-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Bindings
(prelude-swap-meta-and-super)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c '") 'comment-dwim)
(global-set-key (kbd "C-c k") 'kill-region)

(require 'smex)
(smex-initialize)
(require 'ace-jump-mode)

(setq-default tab-width 4)
(global-rainbow-delimiters-mode 1)

(mouse-avoidance-mode 'cat-and-mouse)

(setq
 cursor-in-non-selected-windows nil
 use-dialog-box nil
 whitespace-line-column 150
 whitespace-style '(face
                    trailing
                    lines
                    tabs
                    space-before-tab space-after-tab
                    indentation))

(global-whitespace-mode t)
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
      ac-candidate-limit 10
      ac-ignore-case nil)
(global-auto-complete-mode)

;; Tramp
(setq tramp-default-method "ssh")

;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Erlang
(add-to-list 'load-path "~/.emacs.d/vendor/edts")
(require 'edts-start)
(edts-project-override "~/erl-project" '(:lib-dirs '("lib", "deps", "ebin")))
