;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-scale-headings t)
  (load-theme 'modus-vivendi-tinted))

(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-scale-org-headlines t))

(use-package solarized-theme
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (setq solarized-high-contrast-mode-line t))

(provide 'kw-theme)
