;; -*- lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode)
  (vertico-count 7)
  :init
  (vertico-mode t)
  :config
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :ensure t
  :after vertico
  :commands (orderless)
  :custom (completion-styles '(orderless flex)))

(use-package consult
  :ensure t
  :after vertico
  :bind
  ("C-x b" . consult-buffer)
  ("C-s" . consult-line)
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package corfu
  :ensure t
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t)                    ; Only use `corfu' when calling `completion-at-point' or
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.6)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width) ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  :init
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   ("C-," . embark-act-all)
   :map minibuffer-local-map
   ("C-," . embark-act-all))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;(setq embark-prompter 'embark-completing-read-prompter)
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))

(provide 'init-interface)
