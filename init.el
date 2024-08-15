;;; init.el --- kiwib0y's Emacs configuration

;; Copyright 2021-present, All rights reserved
;; Code licensed under the GNU GPL v.3 license

;; Author: kiwib0y
;; URL: https://github.com/kiwib0y/emacs-config

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
(setq ad-redefinition-action 'accept)               ;; Remove redefinition warning
(setq native-comp-async-report-warnings-errors nil) ;; Remove native compilation error warnings

;; don't store backup files
(setq make-backup-files nil)
;; (setq backup-directory-alist
;;       '(("." . "~/.emacs.d/emacs-backup")))

(setq custom-file
      (expand-file-name "~/.emacs.d/custom/custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; don't ask for symbolic links
(setq vc-follow-symlinks t)

;; fonts configuration
(defvar kw/font-sizes 110)

(defun kw/font-face ()
  "Setup all fonts to Hack font."
  (set-face-attribute 'default nil
          :font "Hack" :height kw/font-sizes)
  (set-face-attribute 'fixed-pitch nil
          :font "Hack" :height kw/font-sizes))

;; Set UTF-8 for easy cross-platform use
(set-default-coding-systems 'utf-8)

;; Set transparent opacity
(set-frame-parameter nil 'alpha-background 78)
(add-to-list 'default-frame-alist '(alpha-background . 78))

;; daemon frame setup
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
    (with-selected-frame frame
      (kw/font-face))))
  (kw/font-face))

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

;; ensure always -> I'm on emacs 30 and it's a native compile
(setq use-package-always-ensure t)
;; initialize package source
(require 'package)
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))


;; check package sources
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

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

;; hide annoying modes in modeline
(use-package diminish)

;; Ivy, Swiper and Counsel
(use-package swiper
  :ensure t)
(use-package counsel
  :ensure t)
(use-package ivy
  :diminish ivy-mode
  :bind (("M-x"     . counsel-M-x)
         ("C-s"     . swiper)
         ("C-x y"   . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x b"   . counsel-switch-buffer)
         ("C-c C-r" . ivy-resume))
  :commands (ivy-set-actions)
  :config
  (ivy-mode))

;; a useful tool for the M-x mode
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
  :config
  (setq dired-listing-switches "-lAGh1v --sort=extension --group-directories-first")
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

(use-package nerd-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

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
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode +1)
  :custom (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; flycheck syntax checking setup
(use-package flycheck
  :ensure t)

(use-package flycheck-joker ;; flycheck for clojure
  :ensure t)

(use-package flycheck-clj-kondo
  :ensure t)

;; lsp-mode setup
;; (defun kw/lsp-mode-setup ()
;;   "Disable the in-project path breadcrumb."
;;   (setq lsp-headerline-breadcrumb-enable nil))

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
  :after lsp-mode
  :ensure t)

(setq lsp-enable-links nil)

(use-package lsp-tailwindcss
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-hook 'lsp-tailwindcss-major-modes 'svelte-mode))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode))

;; rust programming setup
(use-package rustic
  :ensure t
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

(use-package sgml-mode
  :ensure t
  :hook
  (html-mode . (lambda () (setq me/pretty-print-function #'sgml-pretty-print)))
  (html-mode . sgml-electric-tag-pair-mode)
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

;; Eglot setup
(use-package eglot
  :ensure t)

;; python setup
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
  :ensure t
  :mode
  ("\\.py\\'" . python-mode)
  :hook (python-mode . (lambda ()
                          'lsp-pyright
                          (lsp-deferred))))

(use-package pyvenv
  :ensure t
  :after (eglot)
  :init
  ;; set the working home directory.
  ;; create 'versions' directory manualy.
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

;; typescript quickstart
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (setq js-indent-level 2))

(use-package restclient
  :ensure t)

(use-package rjsx-mode
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :hook (rjsx-mode . lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode))
  ;; (setq js2-mode-show-strict-warnings nil)
  ;; (setq js2-strict-trailing-comma-warning nil)
  ;; (setq js2-basic-offset 2)
  (setq js-indent-level 2))

(add-to-list 'auto-mode-alist '("react" . rjsx-mode))


(use-package js2-mode
    :ensure t
    :mode (("\\.js\\'" . js2-mode)
           ("\\.jsx\\'" . js2-jsx-mode)))

(use-package js
  :ensure nil
  :mode ("\\.jsx?\\'" . js-mode)
  :config
  (setq js-indent-level 2))

(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))

;; (use-package prettier-js
;;   :ensure t
;;   :hook ((web-mode . prettier-js-mode)
;;          (typescript-mode . prettier-js-mode)
;;          (js-mode . prettier-js-mode)
;;          (rjsx-mode . prettier-js-mode))
;;   :config
;;   (setq prettier-js-args '("--single-quote"
;;                            "--trailing-comma" "all"
;;                            "--print-width" "100")))

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

(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'"
  :hook
  (svelte-mode . lsp-deferred)
  :config
  (setq svelte-basic-offset 2))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'"
         "\\.css\\'")
  :config
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; paredit mode
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode))

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
  ;; (setq cider-font-lock-dynamically '(macro core function var))
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; terraform setup
(use-package terraform-mode
  ;; terraform + lsp is very buggy for now
  ;; I'd consider using it in the future
  ;; but for now I'm okay with using regular
  ;; clean terraform-mode
  :ensure t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; yaml setup
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'"
         "\\.yaml\\'")
  :config
  (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))

;; json setup
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

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

;; Docker mode
(use-package dockerfile-mode
  :ensure t)

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
  :init
  (setq markdown-command "markdown"))

;; themes
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

;; eshell prompt
(setq eshell-prompt-function
      (lambda ()
        (concat "[" (getenv "USER") "@" (system-name) " "
                (eshell/pwd) "] " (if (= (user-uid) 0) "# " "λ "))))

;; ORG
(defun kw/org-mode-setup ()
  "My personal org mode setup."
  (org-indent-mode t)
  (visual-line-mode 1))

;; disable electric-pair-mode
(add-hook 'org-mode-hook
          (lambda () (electric-pair-mode 0)))

(use-package org
  :defer t
  :hook (org-mode . kw/org-mode-setup)
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
(setq org-clock-sound "~/Music/bell.wav")

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

(defun kw/org-mode-visual-fill ()
  "Center the files for better experience
   in the visual-fill-column mode."
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . kw/org-mode-visual-fill))

;; org-mode agenda setup
(setq org-directory "~/Dropbox/OrgFiles/")
(setq org-agenda-files (list org-directory))
(add-to-list 'org-agenda-files org-directory)

;;; init.el ends here
