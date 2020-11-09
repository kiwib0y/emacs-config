;;; PACKAGE --- Summary

;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(blink-cursor-mode 1)	   ;; Set blinking
(set-fringe-mode 10)	   ;; Give breathing room
(tool-bar-mode -1)       ;; Disable the toolbar
(scroll-bar-mode -1)	   ;; Disable the scrollbar
(electric-pair-mode 1)   ;; Electric pair parenthesis
(show-paren-mode 1)		   ;; Show global parenthesis on all buffers
(column-number-mode 1)   ;; Add column number

(setq inhibit-startup-message t)

(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(load custom-file) 

(setq-default tab-width 2) 
(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(global-set-key (kbd "TAB") 'my-insert-tab-char)

;; Set visible notification bell
(setq visible-bell 'top-bottom)
(setq bell-volume 0)

;; Relative line numbers activation on a global scale
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Shell line-numbers set up
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))

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
  :config
  (ivy-mode 1))    

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-mode-height 15)))

(use-package magit
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))


;;; ORG MODE config
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (java . t)))        

(add-to-list 'load-path
	     "~/.emacs.d/plugins/org-bullets") 
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))) 

(setq org-ellipsis "↴")
;;; ORG MODE ends here


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
