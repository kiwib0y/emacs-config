;; -*- lexical-binding: t; -*-

(use-package flycheck)

(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l' , 's-l' or the other one
  :config
  (setq lsp-enable-which-key-integration t)
  (setq lsp-auto-guess-root nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-folding nil)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-eldoc-enable-hover nil)
  :hook ((js2-jsx-mode
          svelte-mode
          js-mode
          typescript-mode
          c-mode
          c++-mode
          python-mode) . lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-delay 0.8)
  (lsp-ui-sideline-show-hover nil))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode)

(setq lsp-enable-links nil)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package eglot)

(use-package paredit
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("TAB" . company-indent-or-complete-common)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common)
        ("TAB" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1)
  (diminish 'company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode #'rainbow-delimiters-mode))

(add-hook 'prog-mode-hook
          (lambda()
            (define-key prog-mode-map (kbd "M-;") #'comment-or-uncomment-region)))

(provide 'kw-prog-conf)
