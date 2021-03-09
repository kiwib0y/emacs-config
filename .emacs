;;; PACKAGE --- Summary

;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(blink-cursor-mode 1)    ;; Set blinking
(set-fringe-mode 8)      ;; Set the optimal fringe
(tool-bar-mode -1)       ;; Disable the toolbar
(scroll-bar-mode -1)     ;; Disable the scrollbar
(electric-pair-mode 1)   ;; Electric pair parenthesis
(column-number-mode 1)   ;; Add column number
(show-paren-mode 1)      ;; Show global parenthesis on all buffers
(setq inhibit-startup-message t)

(setq custom-file
      (expand-file-name ".custom.el" user-emacs-directory))
(load custom-file)

;; Set tabs to be spaces
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set visible notification bell
(setq visible-bell 'top-bottom)
(setq bell-volume 0)

;; Relative line numbers activation on a global scale
(defvar display-line-numbers-type)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Shell line-numbers set up
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Hook for commenting
(add-hook 'prog-mode-hook
          (lambda()
            (define-key prog-mode-map (kbd "M-;") #'comment-or-uncomment-region)))

;; Initialize package source
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Check package sources
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Scroll only by half
(use-package view
  :bind
       ("C-v" . View-scroll-half-page-forward)
       ("M-v" . View-scroll-half-page-backward))

(use-package ivy
  :diminish
  :bind	("M-x" . 'counsel-M-x)
        ("C-s" . 'swiper)
        ("C-x C-f" . 'counsel-find-file)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; A useful tool for the =M-x= mode
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (define-key dired-mode-map (kbd "f") 'dired-single-buffer)
  (define-key dired-mode-map (kbd "b") 'dired-single-up-directory))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package pdf-tools
  :pin manual
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-mode-height 12)))
(setq doom-modeline-buffer-file-name-style 'auto)

(use-package magit
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Explain what key does
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))

;; Coding config 
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
   (when (file-directory-p "~/Github")
    (setq projectile-project-search-path '("~/Github")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; LSP-mode enabled here
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l' , 's-l' or the other one 
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ivy)

;; LSP-JAVA quickstart
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config (add-hook 'java-mode-hook 'lsp))

(setq lsp-enable-links nil)

(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)

(use-package helm-lsp)


;; Golang mode
(use-package go-mode
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)))


;; TS quickstart
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))


;; Company autocompletion
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         ("M-n" . company-select-next)
         ("M-p" . company-select-previous))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

;; ORG MODE
(defvar org-file
  (setq org-file (expand-file-name ".org-config.el" user-emacs-directory)))
(load org-file)

;; Yasnippet activation here
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(use-package yasnippet-classic-snippets
  :ensure t)

;; Add the dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
(setq dracula-use-24-bit-colors-on-256-colors-terms t)
(unless (display-graphic-p)
  (set-face-background 'default "black" nil))

;;; .emacs ends here
