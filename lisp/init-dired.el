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
    "q" '+dired/quit-all
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
  (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)
  (defun +dired-disable-gnu-ls-flags-maybe-h ()
    "Remove extraneous switches from `dired-actual-switches' when it's
uncertain that they are supported (e.g. over TRAMP or on Windows).

Fixes #1703: dired over TRAMP displays a blank screen.
Fixes #3939: unsortable dired entries on Windows."
    (when (or (file-remote-p default-directory)
              (and (boundp 'ls-lisp-use-insert-directory-program)
                   (not ls-lisp-use-insert-directory-program)))
      (setq-local dired-actual-switches (car args))))
  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (setq dired-listing-switches (string-join args " "))
    (add-hook 'dired-mode-hook #'+dired-disable-gnu-ls-flags-maybe-h))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert))))

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

;;(use-package dirvish)
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


(use-package fd-dired
  :when thomas-projectile-fd-binary
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired))

(provide 'init-dired)
;;; init-dired.el ends here
