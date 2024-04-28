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
        magit-ediff-dwim-show-on-hunks t)
  (add-hook 'magit-process-model-hook #'goto-address-mode))

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))

(with-eval-after-load 'evil-collection
  (use-package evil-collection-magit
    :init (defvar evil-collection-magit-use-z-for-folds t)
    :config
    ;; q is enough; ESC is way too easy for a vimmer to accidentally press,
    ;; especially when traversing modes in magit buffers.
    (evil-define-key* 'normal magit-status-mode-map [escape] nil)

    ;; Some extra vim-isms I thought were missing from upstream
    (evil-define-key* '(normal visual) magit-mode-map
      "*"  #'magit-worktree
      "zt" #'evil-scroll-line-to-top
      "zz" #'evil-scroll-line-to-center
      "zb" #'evil-scroll-line-to-bottom
      "g=" #'magit-diff-default-context)

    ;; Fix these keybinds because they are blacklisted
    ;; REVIEW There must be a better way to exclude particular evil-collection
    ;;        modules from the blacklist.
    (general-def :states '(normal visual) magit-mode-map
      "]" #'magit-section-forward-sibling
      "[" #'magit-section-backward-sibling
      "gr" #'magit-refresh
      "gR" #'magit-refresh-all)

    (general-def :states '(normal visual) magit-status-mode-map
      "gz" #'magit-refresh)

    (general-def :states '(normal visual) magit-diff-mode-map
      "gd" #'magit-jump-to-diffstat-or-diff)

    ;; A more intuitive behavior for TAB in magit buffers:
    (general-def 'normal :keymaps
      '(magit-status-mode-map)
      magit-stash-mode-map
      magit-revision-mode-map
      magit-process-mode-map
      magit-diff-mode-map
      [tab] #'magit-section-toggle)

    (with-eval-after-load 'git-rebase
      (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
        (when-let (desc (assoc (car key) evil-collection-magit-rebase-commands-w-descriptions))
          (setcar desc (cdr key))))
      (evil-define-key* evil-collection-magit-state git-rebase-mode-map
        "gj" #'git-rebase-move-line-down
        "gk" #'git-rebase-move-line-up))))



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
