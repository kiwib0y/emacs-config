;; -*- lexical-binding: t; -*-

(use-package projectile
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

(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :config
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

(provide 'kw-projects)
