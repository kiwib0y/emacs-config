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

(blink-cursor-mode -1)    ;; Disable cursor blink
(column-number-mode 1)    ;; Add column number
(delete-selection-mode 1) ;; Better paste over region behavior
(electric-pair-mode 1)    ;; Electric pair parenthesis
(global-hl-line-mode 1)   ;; Highlight current line
(save-place-mode 1)       ;; Remember the cursor location
(scroll-bar-mode -1)      ;; Disable the scrollbar
(set-fringe-mode 5)       ;; Set the optimal fringe
(show-paren-mode 1)       ;; Show global parenthesis on all buffers
(tool-bar-mode -1)        ;; Disable the toolbar
(menu-bar-mode -1)        ;; Disable the menubar

(setq inhibit-startup-message t)
(setq ad-redefinition-action 'accept) ;; Remove redefinition warning

;; add this inside of custom-set-variables
;; '(org-directory "~/Documents/Org")
;; '(org-agenda-files (list org-directory))
(setq custom-file
      (expand-file-name "~/.emacs.d/custom/custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; don't ask for symbolic links
(setq vc-follow-symlinks t)

;; fonts configuration
(defvar kiwib0y/font-sizes 110)

(defun kiwib0y/font-face ()
  "Setup all fonts to Hack font."
  (set-face-attribute 'default nil
		      :font "Hack" :height kiwib0y/font-sizes)
  (set-face-attribute 'fixed-pitch nil
		      :font "Hack" :height kiwib0y/font-sizes))

;; Set UTF-8 for easy cross-platform use
(set-default-coding-systems 'utf-8)

;; daemon frame setup
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (kiwib0y/font-face))))
  (kiwib0y/font-face))

;; set tabs to be 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq indent-line-function 'insert-tab)

;; set visible notification bell and disable annoying ringing
(setq visible-bell 'top-bottom)
(setq ring-bell-function 'ignore)

;; relative line numbers activation on a global scale
(defvar display-line-numbers-type)
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 4)
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

;; hook for commenting
(add-hook 'prog-mode-hook
          (lambda()
            (define-key prog-mode-map (kbd "M-;") #'comment-or-uncomment-region)))

;; initialize package source
(require 'package)
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; check package sources
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; scrolling smoothly
(use-package good-scroll
  :ensure t
  :config
  (global-set-key (kbd "C-v") #'good-scroll-up)
  (global-set-key (kbd "M-v") #'good-scroll-down)
  (good-scroll-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package windmove
  :config
  (global-set-key (kbd "M-S-<left>") 'windmove-left)
  (global-set-key (kbd "M-S-<right>") 'windmove-right)
  (global-set-key (kbd "M-S-<up>") 'windmove-up)
  (global-set-key (kbd "M-S-<down>") 'windmove-down))

(use-package ivy
  :diminish
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-s" . 'swiper)
  ("C-x C-f" . 'counsel-find-file)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; a useful tool for the M-x mode
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; hide annoying modes in modeline
(use-package diminish)

(use-package all-the-icons
  :ensure t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-dwim-target t)
  (define-key dired-mode-map (kbd "f") 'dired-single-buffer)
  (define-key dired-mode-map (kbd "b") '(lambda () (interactive) (dired-single-buffer ".."))) ;; 'dired-sinlge-up-directory
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (interactive)
	      (dired-hide-details-mode 1))))

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

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch))

(use-package htmlize
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode #'rainbow-delimiters-mode))

;; explain what key does
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))

;; coding setup
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode +1)
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
  :ensure t)

(use-package flycheck-joker ;; flycheck for clojure
  :ensure t)

(use-package flycheck-clj-kondo
  :ensure t)

;; lsp-mode setup
(defun kiwib0y/lsp-mode-setup ()
  "Shows the in-project path as breadcrumb."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

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
  :hook ((js-mode
          js2-mode
          typescript-mode
          web-mode
          c-mode
          kiwib0y/lsp-mode-setup) . lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-hover t))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode
  :ensure t)

(setq lsp-enable-links nil)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
	      ("M-j" . lsp-ui-imenu)
	      ("M-?" . lsp-find-references)))

;; rust programming setup
(use-package rust-mode
  :hook (rust-mode . lsp-deferred)
  :config
  (add-hook 'lsp-rust-server #'rust-analyzer)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "<tab>") #'company-indent-or-complete-common)
  (setq rust-format-on-save t)
  (setq rust-indent-offset 2))

(use-package cargo-mode
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

;; typescript quickstart
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :after lsp
  :hook (js2-mode . lsp-deferred)
  :custom
  (js2-include-node-externs t)
  (js2-global-externs '("customElements"))
  (js2-highlight-level 3)
  (js2r-prefer-let-over-var t)
  (js2r-prefered-quote-type 2)
  (js-indent-align-list-continuation t)
  (global-auto-highlight-symbol-mode t)
  :config
  (setq-default js-indent-level 2)
  (setq-default js2-bounce-indent-p nil))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :hook
  (vue-mode . lsp-deferred)
	:config
	(setq mmm-submode-decoration-level 0)
  (setq-default vue-html-extra-indent 2)
  (setq-default vue-indent-level 2))

(use-package vue-html-mode
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'"
         "\\.css\\'"
         "\\.php\\'"
         "\\.js[x]?\\'")
  :config
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2))

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
  (require 'flycheck-clj-kondo))

(use-package inf-clojure
  :ensure t
  :config
  (add-hook 'inf-clojure-mode-hook #'paredit-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages nil)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-clear-help-banner t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-font-lock-dynamically '(macro core function var))
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package terraform-mode
  :ensure t)

;; company autocompletion setup
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

;; pdf-view setup
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;; yaml setup
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :config
  (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))

;; markdown setup
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;; theme
(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-scale-org-headlines t)
  (load-theme 'zenburn t))

;; ORG
(defun kiwib0y/org-mode-setup ()
  "My personal org mode setup."
  (org-indent-mode t)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . kiwib0y/org-mode-setup)
  :config
  (setq org-ellipsis "↴")
  (setq org-hide-emphasis-markers t)
  (setq org-export-latex-listings t)
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted")))
  (setq org-src-fontify-natively t)


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

(use-package org-appear
  :hook (org-mode . org-appear-mode))

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
  "Center the files for better experience
   in the visual-fill-column mode."
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . kiwib0y/org-mode-visual-fill))

;; org-mode agenda setup
;; (setq org-agenda-files
;;       '("~/.emacs.d/OrgFiles/Agenda.org"))

;; (add-to-list 'load-path
;;              org-agenda-files)
;;; init.el ends here
