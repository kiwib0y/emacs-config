;; -*- lexical-binding: t; -*-

(use-package diminish)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 1.0))))
  (mode-line-inactive ((t (:height 1.0))))
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-height 30)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-bar-width 4)
  (doom-modeline-buffer-modification-icon nil))

(setq doom-modeline-buffer-file-name-style 'auto)

(provide 'kw-modeline)
