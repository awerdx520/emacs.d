;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;
;; git-messenger has been superseded by {C-x v h} (`vc-region-history')

;;; Code:

;; The awesome git client
;; Explicit binding makes it load lazily although it's the default.
;; See `magit-define-global-key-bindings' for more information.
(use-package magit
  :after transient
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :general
  (thomas-leader
   ;;
   "g." 'magit-file-dispatch
   "g/" 'magit-dispatch
   "gb" 'magit-branch-checkout
   "gC" 'magit-clone
   "gD" 'magit-file-delete
   "gg" 'magit-status
   "gG" 'magit-status-here
   "gL" 'magit-log-buffer-file
   "gS" 'magit-stage-buffer-file
   "gU" 'magit-unstage-buffer-file
   ;;
   "pt" 'magit-todos-list) ;; 列出项目 TODO


  :config
  (setq magit-diff-refine-hunk t
        magit-diff-paint-whitespace nil
        magit-ediff-dwim-show-on-hunks t))

;; NOTE: `diff-hl' depends on `vc'
(use-package vc
  :straight (:type built-in)
  :config
  (setq vc-follow-symlinks t
        vc-allow-async-revert t
        vc-handled-backends '(Git)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

;; Visual diff interface
(use-package ediff
  :straight (:type built-in)
  ;; Restore window config after quitting ediff
  :hook ((ediff-before-setup . ediff-save-window-conf)
         (ediff-quit         . ediff-restore-window-conf))
  :config
  (defvar local-ediff-saved-window-conf nil)

  (defun ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))
  :config
  (setq ediff-highlight-all-diffs t
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Setup gitignore mode
(use-package conf-mode
  :straight (:type built-in)
  :mode (("\\.gitignore\\'"     . conf-unix-mode)
         ("\\.gitconfig\\'"     . conf-unix-mode)
         ("\\.gitattributes\\'" . conf-unix-mode)))

(provide 'init-git)

;;; init-git.el ends here
