;;; PACKAGE --- Summary

;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(electric-pair-mode 1) ;; Electric pair parenthesis
(show-paren-mode 1)    ;; Show global parenthesis on all buffers
(set-fringe-mode 10)   ;; Give breathing room

;; Set visible notification bell
(setq visible-bell 'top-bottom)
(setq bell-volume 0)

;; Relative line numbers activation on a global scale
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

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


(use-package hydra) ;; Add hydra
(use-package ivy
  :config
  (ivy-mode 1))     ;; Add and toggle ivy


;;; ORG MODE config

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)))                  


(add-to-list 'load-path
	     "~/.emacs.d/plugins/org-bullets") ;; Add new org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))) 

;;; ORG MODE ends here


;; Yasnippet activation here
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


;; Add the dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
(setq dracula-use-24-bit-colors-on-256-colors-terms t)
(unless (display-graphic-p)
  (set-face-background 'default "black" nil))

;;; .emacs ends here
