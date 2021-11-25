;;; init.el --- kiwib0y's Emacs configuration

;; Copyright 2021-present, All rights reserved
;; Code licensed under the GNU GPL v.3 license

;; Author: KiwiB0y
;; URL: https://github.com/KiwiB0y/emacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my current Emacs configuration
;; Keep in mind that the configuration works with GNU Emacs 27+

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
(setq ad-redefinition-action 'accept) ;; Remove redefinition warning

(setq custom-file
      (expand-file-name "~/.emacs.d/custom/custom.el" user-emacs-directory))
(load custom-file)

;; set tabs to be spaces
(setq tab-width 2)
(setq-default indent-tabs-mode t)

;; set visible notification bell
(setq visible-bell 'top-bottom)

;; relative line numbers activation on a global scale
(defvar display-line-numbers-type)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; line-numbers set up for different modes
(dolist (mode '(shell-mode-hook
		eshell-mode-hook
		term-mode-hook
		treemacs-mode-hook
		org-mode-hook
		pdf-view-mode-hook
		image-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode 0)))
;; (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))
;; (add-hook 'term-mode-hook (lambda () (display-line-numbers-mode 0)))
;; (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode 0)))
;; (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
;; (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))
;; (add-hook 'image-mode-hook (lambda () (display-line-numbers-mode 0)))

;; hook for commenting
(add-hook 'prog-mode-hook
          (lambda()
            (define-key prog-mode-map (kbd "M-;") #'comment-or-uncomment-region)))

;; initialize package source
(require 'package)
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

;; check package sources
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))
;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; scroll only by half
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

;; a useful tool for the =M-x= mode
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

(use-package emojify
  :hook (markdown-mode . emojify-mode))

(use-package multiple-cursors)
(global-set-key (kbd "C-c c m") 'mc/edit-lines)

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
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package htmlize
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; explain what key does
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))

;; coding setup
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

;; flycheck syntax checking setup
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package flycheck-joker ;; flycheck for clojure
  :ensure t)

;; lsp-mode setup
(defun kiwib0y/lsp-mode-setup ()
  "Here is my lsp-mode-setup function."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . kiwib0y/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l' , 's-l' or the other one
  :config
  (setq lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode
  :ensure t)

(setq lsp-enable-links nil)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

;; lsp-dart mode setup
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp-mode))

;; optional flutter packages
(use-package hover
  :ensure t) ;; run app from desktop without emulator

;; python programming setup
(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

;; rust programming setup
(use-package rust-mode
  :hook (rust-mode . lsp-deferred)
  :config
  (add-hook 'lsp-rust-server #'rust-analyzer)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq rust-format-on-save t)
  (setq rust-indent-offset 4))

(use-package cargo-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; golang mode setup
(use-package go-mode
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)))

;; typescript quickstart
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'"
         "\\.css\\'"
         "\\.php\\'"
         "\\.js[x]?\\'"))

;; paredit mode
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;; clojure programming setup
(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package inf-clojure
  :ensure t
  :config
  (add-hook 'inf-clojure-mode-hook #'paredit-mode)
  (add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; company autocompletion setup
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
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0)
  (diminish 'company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

;; pdf-view setup
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;; markdown setup
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; org-mode setup
(defun kiwib0y/org-mode-setup ()
  "My personal org mode setup function."
  (org-indent-mode t)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . kiwib0y/org-mode-setup)
  :config
  (setq org-ellipsis "↴")
  (setq org-hide-emphasis-markers t)

  (org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (java . t)
   (python . t)
   (ruby . t))))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq latex-run-command "pdflatex")

(require 'org-tempo)

(use-package org-tree-slide
	:custom
	(org-image-actual-width nil))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●" "○")))

(with-eval-after-load 'org-faces
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.32)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))

(defun kiwib0y/org-mode-visual-fill ()
  "Here is my org-mode-visual-fill function."
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . kiwib0y/org-mode-visual-fill))

;; org-mode agenda setup
(setq org-agenda-files
      '("~/.emacs.d/OrgFiles/Tasks.org"))

(add-to-list 'load-path
             org-agenda-files)

(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-scale-org-headlines t)
  (load-theme 'zenburn t))
;;; init.el ends here
