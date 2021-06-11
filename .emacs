;;; .emacs --- kiwib0y's Emacs configuration

;; Copyright 2021-present, All rights reserved
;; Code licensed under the GNU GPL v.3 license

;; Author: KiwiB0y
;; URL: https://github.com/KiwiB0y/emacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my current Emacs configuration

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(blink-cursor-mode 1)           ;; Set blinking
(set-fringe-mode 8)             ;; Set the optimal fringe
(tool-bar-mode -1)              ;; Disable the toolbar
(scroll-bar-mode -1)            ;; Disable the scrollbar
(electric-pair-mode 1)          ;; Electric pair parenthesis
(column-number-mode 1)          ;; Add column number
(show-paren-mode 1)             ;; Show global parenthesis on all buffers
(global-hl-line-mode 1)         ;; Highlight current line
(setq inhibit-startup-message t)


(setq custom-file
      (expand-file-name "~/.emacs.d/kiwib0y-custom/custom.el" user-emacs-directory))
(load custom-file)

;; Set tabs to be spaces
(setq tab-width 2)
(setq-default indent-tabs-mode t)

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

(use-package all-the-icons
  :ensure t)

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

(use-package multiple-cursors)
(global-set-key (kbd "C-c c m") 'mc/edit-lines)

(use-package pdf-tools
  :pin manual
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(use-package autothemer
  :ensure t)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 1.0))))
  (mode-line-inactive ((t (:height 1.0))))
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-mode-height 12))

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

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java :ensure nil)

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

(use-package web-mode
  :mode "\\.js[x]?\\'")

(use-package rainbow-mode
  :ensure t)

;; TO BE CONTINUED!
;; js mode

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
  (setq org-file (expand-file-name "~/.emacs.d/kiwib0y-org/org-config.el" user-emacs-directory)))
(load org-file)

;; Yasnippet activation here
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(use-package yasnippet-classic-snippets
  :ensure t)

;; currently what I am working on <---
;; load my custom theme
;; (load-theme 'antim t)

;; Add the dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
(setq dracula-use-24-bit-colors-on-256-colors-terms t)
(unless (display-graphic-p)
  (set-face-background 'default "black" nil))

;;; .emacs ends here
