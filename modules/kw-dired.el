;; -*- lexical-binding: t; -*-

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
  (define-key dired-mode-map (kbd "b") 'dired-single-up-directory)
  (add-hook 'dired-mode-hook
      (lambda ()
        (interactive)
        (dired-hide-details-mode 1))))

(use-package dired-single)

(use-package nerd-icons-dired
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package emojify
  :hook (markdown-mode . emojify-mode))

(provide 'kw-dired)
