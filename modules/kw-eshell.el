;; -*- lexical-binding: t; -*-

(setq eshell-prompt-function
      (lambda ()
        (concat "[" (getenv "USER") "@" (system-name) " "
                (eshell/pwd) "] " (if (= (user-uid) 0) "# " "λ "))))

(provide 'kw-eshell)
