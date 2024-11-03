;;; init-tramp.el tramp setting -*- lexical-binding: t; -*-
;;
;; transparent remote access
(use-package tramp
  :defer t
  :straight (:type built-in)
  :config
  (setq tramp-verbose 1
        tramp-default-method "ssh"
        tramp-completion-reread-directory-timeout 60
        tramp-persistency-file-name (expand-file-name "tramp" thomas-cache-dir))

  (setq remote-file-name-inhibit-cache 60
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")))

(provide 'init-tramp)
