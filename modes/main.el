(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(flyspell-mode-off)
(setq prelude-guru nil)
(setq prelude-flyspell nil)
(global-flycheck-mode -1)

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
