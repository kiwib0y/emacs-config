;;; package --- Summary

;;; Commentary:
;;; Here is my ORG MODE config

;;; Code:
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (java . t)
   (python . t)
   (ruby . t)))

(add-to-list 'load-path
	     "~/.emacs.d/plugins/org-bullets")
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-ellipsis "↴")

(use-package org-tree-slide
	:custom
	(org-image-actual-width nil))


;;; .org-config.el ends here
