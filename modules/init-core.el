;; -*- lexical-binding: t; -*-

(blink-cursor-mode -1)                              ;; Disable cursor blink
(column-number-mode 1)                              ;; Add column number
(delete-selection-mode 1)                           ;; Better paste over region behavior
(electric-pair-mode 1)                              ;; Electric pair parenthesis
(global-hl-line-mode 1)                             ;; Highlight current line
(save-place-mode 1)                                 ;; Remember the cursor location
(scroll-bar-mode -1)                                ;; Disable the scrollbar
(set-fringe-mode 2)                                 ;; Set the optimal fringe
(show-paren-mode 1)                                 ;; Show global parenthesis on all buffers
(tool-bar-mode -1)                                  ;; Disable the toolbar
(menu-bar-mode -1)                                  ;; Disable the menubar

(setq inhibit-startup-message t)
(setq ad-redefinition-action 'accept)               ;; Remove redefinition warning
(setq native-comp-async-report-warnings-errors nil) ;; Remove native compilation error warnings
(setq switch-to-buffer-obey-display-actions t)

(setq make-backup-files nil)
(setq vc-follow-symlinks t)


(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)


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
                org-agenda-mode-hook
                pdf-view-mode-hook
                image-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; set UTF-8 for easy cross-platform use
(set-default-coding-systems 'utf-8)

;; fonts configuration
(defvar kw/font-sizes 110)

(defun kw/font-face ()
  "Setup all fonts to Hack font."
  (set-face-attribute 'default nil
          :font "Hack" :height kw/font-sizes)
  (set-face-attribute 'fixed-pitch nil
          :font "Hack" :height kw/font-sizes))

;; daemon frame setup
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (kw/font-face))))
  (kw/font-face))

;; set visible notification bell and disable annoying ringing
(setq visible-bell 'top-bottom)
(setq ring-bell-function 'ignore)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c c m") 'mc/edit-lines))

(use-package good-scroll
  :config
  (global-set-key (kbd "C-v") #'good-scroll-up)
  (global-set-key (kbd "M-v") #'good-scroll-down)
  (good-scroll-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package windmove
  :config
  (global-set-key (kbd "C-s-h") 'windmove-left)
  (global-set-key (kbd "C-s-l") 'windmove-right)
  (global-set-key (kbd "C-s-k") 'windmove-up)
  (global-set-key (kbd "C-s-j") 'windmove-down))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package password-store)
(use-package tldr)
(use-package htmlize)
(use-package sudo-edit)
(use-package diminish)

(provide 'init-core)
