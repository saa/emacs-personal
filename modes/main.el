(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(flyspell-mode-off)
(setq prelude-guru nil)
(setq prelude-flyspell nil)
(global-flycheck-mode -1)

(set-frame-font "Menlo-14")
(set-fontset-font "fontset-default" 'cyrillic '("menlo" . "ISO10646-1"))
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables '(shell-file-name "zsh"))
(ido-vertical-mode)
(setq-default tab-width 4)

(require 'auto-complete)

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
 whitespace-line-column 120
 whitespace-style '(face
                    trailing
                    lines
                    lines-tail
                    tabs
                    space-before-tab space-after-tab
                    indentation))

(require 'protobuf-mode)
