(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(prelude-require-packages '(edts smex auto-complete
                                 golden-ratio rust-mode
                                 solarized-theme
                                 ido-vertical-mode
                                 autopair
                                 alchemist
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

;; (global-flycheck-mode -1)

(set-frame-font "Menlo-14")
(set-fontset-font "fontset-default" 'cyrillic '("menlo" . "ISO10646-1"))

(setq prelude-clean-whitespace-on-save nil)

(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables '(shell-file-name "zsh"))
(ido-vertical-mode)

(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

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
(add-hook 'erlang-mode-hook 'prelude-enable-whitespace)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (require 'edts-start))

(add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("sys.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("app.config" . erlang-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("*.jsx" . web-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint, html-tidy and css-csslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'html-tidy 'web-mode)
(flycheck-add-mode 'css-csslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(defun my-js2-mode-hook ()
  (setq-default js2-basic-offset 2))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; Elixir
(require 'alchemist)

;; Clojure
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
