(defun kw/org-mode-setup ()
  "My personal org mode setup."
  (org-indent-mode t)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook ((org-mode . kw/org-mode-setup)
         (org-mode . (lambda () (electric-pair-mode 0))))

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

(defun kw/org-mode-visual-fill ()
  "Center the files for better experience
   in the visual-fill-column mode."
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook ((org-mode . kw/org-mode-visual-fill)
         (org-agenda-mode . kw/org-mode-visual-fill)))

;; org-mode agenda setup
(setq org-directory "~/Dropbox/OrgFiles/")
(setq org-agenda-files (list org-directory))
(add-to-list 'org-agenda-files org-directory)

(provide 'kw-org)
