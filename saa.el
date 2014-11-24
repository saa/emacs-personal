(prelude-require-packages '(edts smex auto-complete
                                 golden-ratio rust-mode
                                 solarized-theme
                                 ido-vertical-mode
                                 exec-path-from-shell))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(flyspell-mode-off)
(setq prelude-guru nil)
(setq prelude-flyspell nil)
(global-flycheck-mode -1)

(disable-theme 'zenburn)

(setq solarized-distinct-fringe-background t)
(setq solarized-use-less-bold t)
(setq solarized-emphasize-indicators nil)
(load-theme 'solarized-dark t)

(set-frame-font "Menlo-14")
(set-fontset-font "fontset-default" 'cyrillic '("menlo" . "ISO10646-1"))

(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables '(shell-file-name "zsh"))
(ido-vertical-mode)
(setq-default tab-width 4)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'auto-complete)
(setq ac-quick-help-delay 0.5)

(require 'smex)
(smex-initialize)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'ace-jump-mode)
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "s-<up>") 'move-text-up)
(global-set-key (kbd "s-<down>") 'move-text-down)
(global-set-key (kbd "s-f") 'projectile-find-file)
(eval-after-load 'prelude-mode
  '(define-key prelude-mode-map (kbd "s-'") 'comment-dwim))
;; Magit
(eval-after-load 'prelude-mode
  '(define-key prelude-mode-map (kbd "s-m co") 'magit-checkout))
(eval-after-load 'prelude-mode
  '(define-key prelude-mode-map (kbd "s-m pl") 'magit-pull))

;; Erlang
(add-hook 'erlang-mode-hook (lambda () (autopair-mode)))
(add-hook 'erlang-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'erlang-mode-hook 'font-lock-comment-annotations)
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

(setq erlang-indent-level 4)
(setq erlang-argument-indent 4)
(setq erlang-indent-guard 4)

(add-to-list 'ac-modes 'erlang-mode)
(require 'edts-start)

(add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("sys.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("app.config" . erlang-mode))
