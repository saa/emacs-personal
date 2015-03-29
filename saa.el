(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(prelude-require-packages '(edts smex auto-complete
                                 golden-ratio rust-mode
                                 solarized-theme
                                 ido-vertical-mode
                                 autopair
                                 exec-path-from-shell))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Prelude
(setq prelude-guru nil)
(setq prelude-flyspell nil)

(disable-theme 'zenburn)
(load-theme 'solarized-dark t)

(global-flycheck-mode -1)

(set-frame-font "SourceCode Pro Lighth-14")

(setq prelude-clean-whitespace-on-save nil)

(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables '(shell-file-name "zsh"))
(ido-vertical-mode)
(setq-default tab-width 4)

(require 'smex)
(smex-initialize)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

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

;; Functions
(defun font-lock-comment-annotations ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

;; Hooks
(remove-hook 'mouse-leave-buffer-hook #'prelude-auto-save-command)
(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "s-<up>") 'move-text-up)
(global-set-key (kbd "s-<down>") 'move-text-down)
(global-set-key (kbd "s-f") 'projectile-find-file)
(eval-after-load 'prelude-mode
  '(define-key prelude-mode-map (kbd "s-'") 'comment-dwim))

;; Magit bindings
(eval-after-load 'prelude-mode
  '(define-key prelude-mode-map (kbd "s-m co") 'magit-checkout))
(eval-after-load 'prelude-mode
  '(define-key prelude-mode-map (kbd "s-m pl") 'magit-pull))

;; Erlang
(add-hook 'erlang-mode-hook (lambda () (autopair-mode)))
(add-hook 'erlang-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'erlang-mode-hook 'font-lock-comment-annotations)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (require 'edts-start))

(add-to-list 'ac-modes 'erlang-mode)

(add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("sys.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("app.config" . erlang-mode))

;; Elixir
(require 'alchemist)
