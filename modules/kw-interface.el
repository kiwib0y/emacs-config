;; -*- lexical-binding: t; -*-

(use-package swiper)
(use-package counsel)

(use-package ivy
  :diminish ivy-mode
  :bind (("M-x"     . counsel-M-x)
         ("C-s"     . swiper)
         ("C-x y"   . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume))
  :commands (ivy-set-actions)
  :config
  (ivy-mode))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))

(provide 'kw-interface)
