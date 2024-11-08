;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;
;; dired-narrow is superseded by `consult-focus-lines'.


;;; Code:
(use-package dired
  :straight (:type built-in)
  :init
  (setq dired-dwim-target t ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; 生成对齐的 dired 图标
        dired-listing-switches "-ahl -v --group-directories-first --time-style=long-iso"
        ;; Where to store image caches
        image-dired-dir (concat thomas-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)

  :config
  (with-eval-after-load 'evil-collection-dired
    (defun +thomas/dired-copy-absolute-path()
      "Copy absolute path in Dired."
      (interactive)
      (dired-copy-filename-as-kill 0))

    (general-def :states 'normal :keymaps 'dired-mode-map
      "W" '+thomas/dired-copy-absolute-path))

  ;; 修改自 https://www.emacswiki.org/emacs/DiredOmitMode
  (define-advice dired-do-print (:override (&optional _))
    "Show/hide dotfiles."
    (interactive)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (dired-mark-files-regexp "^\\.")
          (dired-do-kill-lines))
      (revert-buffer)
      (setq-local dired-dotfiles-show-p t))

    ;; Don't complain about this command being disabled when we use it
    (put 'dired-find-alternate-file 'disabled nil)))

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
;;
(use-package dired-rsync
  :general (:keymaps 'dired-mode-map "C-c C-r" 'dired-rsync))

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
  (thomas-localleader-define :keymaps 'dired-mode-map
                             "h" 'dired-omit-mode))

(use-package dired-git-info
  :after dired
  :general (:states '(normal global) :keymaps 'dired-mode-map
                    ")" 'dired-git-info-mode))

;; 使用 fd 搜索
(use-package fd-dired
  :when (executable-find "fd")
  :bind ([remap find-dired] . fd-dired))

(provide 'init-dired)
;;; init-dired.el ends here
