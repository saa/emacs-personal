;; Disable modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(flyspell-mode-off)
(setq prelude-guru nil)
(setq prelude-flyspell nil)
(global-flycheck-mode -1)

;; Hooks
(remove-hook 'mouse-leave-buffer-hook #'prelude-auto-save-command)
(remove-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prelude-coding-hook 'flyspell-prog-mode)
(remove-hook 'text-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'prog-mode-hook 'disable-flyspell-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Main
(set-default-font "Menlo-13")
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(global-rainbow-delimiters-mode 1)
(mouse-avoidance-mode 'cat-and-mouse)
(custom-set-variables '(shell-file-name "zsh"))

(setq-default cursor-in-non-selected-windows nil
              use-dialog-box nil
              indent-tabs-mode nil
              tab-width 4
              case-fold-search t
              default-directory "~"
              fill-column 100)

(pending-delete-mode t)
(electric-indent-mode)
(electric-pair-mode t)

(require 'smex)
(smex-initialize)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'ace-jump-mode)

;; Functions
(defun disable-flyspell-mode ()
  (flyspell-mode -1))

(defun insert-erl-separator ()
  (interactive)
  (insert "
%%==============================================================================
%% Example
%%==============================================================================
")
  (next-line))

(defun move-line-up ()
  (interactive)
  (move-line -1))

(defun move-line-down ()
  (interactive)
  (move-line 1))

(defun text-scale-normal-size ()
  (interactive)
  (text-scale-increase 0))

;; Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c '") 'comment-dwim)
(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "C-c l") 'kill-whole-line)
(global-set-key (kbd "C-c \\") 'insert-erl-separator)
(global-set-key (kbd "<s-up>") 'move-line-up)
(global-set-key (kbd "<s-down>") 'move-line-down)
(global-set-key (kbd "C-c <up>") 'move-text-up)
(global-set-key (kbd "C-c <down>") 'move-text-down)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-normal-size)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c C-j") 'join-line)

;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-compish-file "~/.emacs.d/ac-comphist.dat"
      ac-candidate-limit 20
      ac-ignore-case nil)
(global-auto-complete-mode)

;; Diminish keeps the modeline tidy
(require 'diminish)

;; Highlight
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; Ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Clean up obsolete buffers automatically
(require 'midnight)

;; Automatic resizing buffers
(require 'golden-ratio)
(golden-ratio-mode)
(diminish 'golden-ratio-mode)

;; Sensible undo
(require 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)
(defalias 'redo 'undo-tree-redo)

;; Meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; IDO
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)
(ido-mode 'both)
(ido-everywhere t)
(ido-ubiquitous-mode +1)
(flx-ido-mode +1)
(setq ido-use-faces nil)
(setq ido-case-fold t
      ido-confirm-unique-completion nil
      ido-enable-flex-matching nil
      ido-enable-prefix nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/ido.last"
      ido-default-file-method 'selected-window)

;; Paren
(require 'paren)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "#6f6f6f")
(set-face-foreground 'show-paren-match-face "#94bff3")

;; Whitespace
(require 'whitespace)
(setq
 whitespace-line-column 80
 whitespace-style '(face
                    trailing
                    lines
                    lines-tail
                    tabs
                    space-before-tab space-after-tab
                    indentation))
(global-whitespace-mode t)

;; Tramp
(setq tramp-default-method "ssh")

;; Magit
(require 'magit)
(setq magit-emacsclient-executable nil)

;; Erlang
(add-to-list 'load-path "~/.emacs.d/vendor/edts")
(require 'edts-start)
(setq erlang-indent-level 4)
(setq erlang-argument-indent 4)
(setq erlang-indent-guard 4)
