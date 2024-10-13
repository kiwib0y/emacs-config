;; -*- lexical-binding: t; -*-

(use-package lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-hook 'lsp-tailwindcss-major-modes 'svelte-mode))

(use-package svelte-mode
  :mode "\\.svelte\\'"
  :hook
  (svelte-mode . lsp-deferred)
  :config
  (setq svelte-basic-offset 2))

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook
  (vue-mode . lsp-deferred)
  :config
  (setq mmm-submode-decoration-level 0)
  (setq-default vue-html-extra-indent 2)
  (setq-default vue-indent-level 2))

(use-package vue-html-mode
  :defer t)

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (setq js-indent-level 2))

(use-package restclient)

(use-package js2-mode
    :mode (("\\.js\\'" . js2-mode)
           ("\\.jsx\\'" . js2-jsx-mode)))

(use-package js
  :mode ("\\.jsx?\\'" . js-mode)
  :config
  (setq js-indent-level 2))

(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))

(use-package web-mode
  :defer t
  :mode ("\\.html\\'"
         "\\.css\\'")
  :config
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package sgml-mode
  :hook
  (html-mode . (lambda () (setq me/pretty-print-function #'sgml-pretty-print)))
  (html-mode . sgml-electric-tag-pair-mode)
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

(use-package rustic
  :bind
  (:map rustic-mode-map
        ("M-j" . lsp-ui-imenu)
        ("M-?" . lsp-find-references))
  :config
  (require 'lsp-rust)
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
  (setq rust-indent-offset 4))

(use-package cargo-mode
  :config
  (add-hook 'rustic-mode-hook #'cargo-minor-mode))

(use-package python
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  ;; :hook
  ;; (python-mode . eglot-ensure)
  ;; (python-mode . eldoc-mode)
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nil))

(use-package lsp-pyright
  :mode
  ("\\.py\\'" . python-mode)
  :hook (python-mode . (lambda ()
                          'lsp-pyright
                          (lsp-deferred))))

(use-package pyvenv
  :after (eglot)
  :init
  ;; set the working home directory.
  ;; create 'versions' directory manualy.
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (require 'flycheck-clj-kondo))

(use-package inf-clojure
  :config
  (add-hook 'inf-clojure-mode-hook #'paredit-mode))

(use-package cider
  :config
  (setq nrepl-log-messages nil)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-clear-help-banner t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-font-lock-dynamically '(macro core function var))
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package go-mode
  :hook
  (go-mode . lsp-deferred)
  :config
  (setq indent-tabs-mode nil
        tab-width 4))

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package yaml-mode
  :mode ("\\.yml\\'"
         "\\.yaml\\'")
  :config
  (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))

;; json setup
(use-package json-mode
  :mode "\\.json\\'")

(use-package dockerfile-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"))

(provide 'kw-dev)
