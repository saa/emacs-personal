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

;; Main
(disable-theme 'zenburn)
(load-theme 'solarized-light t)
(set-default-font "Menlo-14")
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(global-rainbow-delimiters-mode 1)
(mouse-avoidance-mode 'cat-and-mouse)
(custom-set-variables '(shell-file-name "zsh"))

(setq-default cursor-in-non-selected-windows nil
              indent-tabs-mode nil
              use-dialog-box nil
              tab-width 4
              case-fold-search t
              default-directory "~"
              fill-column 100)

(pending-delete-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

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

(defun insert-erl-fun-separator ()
  (interactive)
  (insert "
%%------------------------------------------------------------------------------
%% @doc
%% Doc example.
%% @end
%%
%%------------------------------------------------------------------------------
")
  (next-line))

(defun insert-erl-header ()
  (interactive)
  (goto-line 1)
  (insert "
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Example <example@example.com>
%% @copyright (C) 2013, Example
%% @doc
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"))

(defun move-line-up ()
  (interactive)
  (move-line -1))

(defun move-line-down ()
  (interactive)
  (move-line 1))

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
(global-set-key (kbd "C-c l") 'kill-whole-line)
(global-set-key (kbd "C-c \\") 'insert-erl-separator)
(global-set-key (kbd "C-c ;") 'insert-erl-fun-separator)
(global-set-key (kbd "<s-up>") 'move-line-up)
(global-set-key (kbd "<s-down>") 'move-line-down)
(global-set-key (kbd "C-c <up>") 'move-text-up)
(global-set-key (kbd "C-c <down>") 'move-text-down)
(global-set-key (kbd "C-0") 'text-scale-normal-size)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c C-j") 'join-line)

(require 'smex)
(smex-initialize)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'ace-jump-mode)
(require 'auto-complete)
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

;; IDO
(require 'ido)
(ido-mode 'both)
(ido-everywhere t)
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
 whitespace-line-column 150
 whitespace-style '(face
                    trailing
                    lines
                    lines-tail
                    tabs
                    space-before-tab space-after-tab
                    indentation))
(global-whitespace-mode t)

;; Magit
(require 'magit)
(setq magit-emacsclient-executable nil)

;; Erlang
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.12/emacs"
                       load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(add-to-list 'ac-modes 'erlang-mode)
(add-hook 'erlang-mode-hook 'electric-pair-mode)
;;(Add-Hook 'erlang-mode-hook 'paredit-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/edts")
(require 'edts-start)
;; (setq erlang-indent-level 4)
;; (setq erlang-argument-indent 4)
;; (setq erlang-indent-guard 4)

;; projectile


;; Protocol Buffer
(require 'protobuf-mode)

;; Ocaml
;; -- Tuareg mode -----------------------------------------
(require 'tuareg)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode))
          auto-mode-alist))

;; -- Tweaks for OS X -------------------------------------
;; Tweak for problem on OS X where Emacs.app doesn't run the right
;; init scripts when invoking a sub-shell
(cond
 ((eq window-system 'ns) ; macosx
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")))

;; -- opam and utop setup --------------------------------
;; Setup environment variables using opam
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
;; The following simpler alternative works as of opam 1.1
;; (dolist
;;    (var (car (read-from-string
;;         (shell-command-to-string "opam config env --sexp"))))
;;  (setenv (car var) (cadr var)))
;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))
;; Update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
              "/../../share/emacs/site-lisp") load-path)
;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

;; -- merlin setup ---------------------------------------

(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode)
;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
;; by spaces.
(define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
(set-face-background 'merlin-type-face "#88FF44")

;; -- enable auto-complete -------------------------------
;; Not required, but useful along with merlin-mode
(require 'auto-complete)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)

(load-file (expand-file-name "~/.emacs.d/vendor/ocp-indent.el"))

;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
