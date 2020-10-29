;;; PACKAGE --- Summary

;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:
(package-initialize)

;; Adding Stable package archive for melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Check package sources
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
;(unless package-installed-p 'use-package
;	(package-install 'use-package))

;(require 'use-package)
;(setq use-package-always-ensure t)

;; Electric pair parenthesis
(electric-pair-mode 1)

;; Show global parenthesis on all buffers
(show-paren-mode 1)

;; Relative line numbers activation on a global scale
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Set visible notification bell
(setq visible-bell 'top-bottom)
(setq bell-volume 0)

;;; ORG MODE config
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)))

;; Add new org bullets
(add-to-list 'load-path
	     "~/.emacs.d/plugins/org-bullets")
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here
