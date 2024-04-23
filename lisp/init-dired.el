;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;
;; dired-narrow is superseded by `consult-focus-lines'.

;;; Code:
(use-package dired
  :straight (:type built-in)
  :general
  (general-def :keymaps 'dired-mode-map
    :states '(normal global)
    "C-c C-e" 'wdired-change-to-wdired-mode)
  :init
  (setq dired-dwim-target t ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat thomas-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil))

;;
(use-package dired-aux
  :straight (:type built-in)
  :after dired
  :config
  (with-no-warnings
    (defvar dired-dotfiles-show t)
    (defun dired-dotfiles-toggle (&rest _)
      "Show/hide dotfiles."
      (interactive)
      (if (not dired-dotfiles-show)
          (revert-buffer)
        (dired-mark-files-regexp "^\\.")
        (dired-do-kill-lines))
      (setq-local dired-dotfiles-show (not dired-dotfiles-show)))

    (advice-add 'dired-do-print :override #'dired-dotfiles-toggle))
  :init
  (setq dired-vc-rename-file t
        dired-do-revert-buffer t
        dired-create-destination-dirs 'ask))

;; Make dired colorful
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Use nerd-icon
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;
(use-package dired-x
  :straight (:type built-in)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd "xdg-open")
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  :general
  (thomas-localleader :keymaps 'dired-mode-map
                      "h" 'dired-omit-mode))

;; 使用 fd 搜索
(use-package fd-dired
  :when (executable-find "fd")
  :init
  (global-set-key [remap find-dired] #'fd-dired))

(provide 'init-dired)
;;; init-dired.el ends here
