;;; PACKAGE --- Summary

;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(blink-cursor-mode 1)    ;; Set blinking
(set-fringe-mode 8)      ;; Give breathing room
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

;; Initialize package source
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             '("org" . "https://orgmode.org/elpa/"))

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
  
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-mode-height 1)))


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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

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

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

;;; ORG MODE
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
