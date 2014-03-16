(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

(setq erlang-indent-level 4)
(setq erlang-argument-indent 4)
(setq erlang-indent-guard 4)

(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'load-path "~/.emacs.d/vendor/edts")
(require 'edts-start)
