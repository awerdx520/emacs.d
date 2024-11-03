;;; init-completion.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package vertico
  :hook (after-init . vertico-mode)
  :general
  (thomas-leader-define
    "'" 'vertico-repeat)
  ;;
  (:states 'insert :keymaps 'vertico-map
           "DEL" 'vertico-directory-delete-char
           "M-RET" 'vertico-exit-input
           "C-j"   'vertico-next
           "C-M-j" 'vertico-next-group
           "C-k"   'vertico-previous
           "C-M-k" 'vertico-previous-group)
  :config
  (setq vertico-sort-function #'vertico-sort-history-length-alpha
        vertico-cycle t
        vertico-count 17)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

;;
;;; Advanced completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        ;; 此外，需要首先尝试 basic 完成样式（而不是作为后备），TRAMP 主机名完成才能正常工作。
        ;; partial-completion 样式允许您使用通配符来完成文件和部分路径，例如， /u/s/l 代表 /usr/share/local 。
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))

  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
        (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  (add-hook 'minibuffer-exit-hook 'disable-py-search))

;;
;;; Minibuffer actions and context menu
(use-package embark
  :bind (:map minibuffer-local-map
              ("C-;"     . embark-act)
              ("C-c C-c" . embark-export)
              ("C-c C-o" . embark-collect))
  :general
  (thomas-leader-define
    "a" '(embark-act :wk "Actions"))
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

;;
;;; Useful search and navigation commands
(use-package consult
  :general
  (thomas-leader-define
    ;; top
    "." 'consult-find
    "RET" 'consult-bookmark
    ;; buffer
    "bb" 'consult-buffer
    "bB" 'consult-buffer-other-window
    ;; file
    "fr" 'consult-recent-file
    ;; help
    "ht" 'consult-theme
    ;; insert
    "ir" 'consult-register
    "is" 'consult-yasnippet
    ;; project
    "pb" 'consult-project-buffer ;; 切换到项目中已经打开的 Buffer
    "pg" 'consult-git-grep
    ;; search
    "sf" 'consult-locate
    "si" 'consult-imenu
    "sI" 'consult-imenu-multi
    "sm" 'consult-bookmark
    "sr" 'consult-mark)
  :config
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;;
  (setq consult-fontify-preserve nil
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window))


;;
(use-package consult-dir
  :defer t
  :general
  (:keymaps 'global [remap list-directory] #'consult-dir)
  (:keymaps 'vertico-map
            "C-x C-d" #'consult-dir
            "C-x C-j" #'consult-dir-jump-file))


;; flycheck
(use-package consult-flycheck
  :after (consult flycheck))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :defer t
  :config
  ;; Batch operation
  (defun embark-export-write ()
    "Export the current vertico results to a writeable bufer if possible.
Supports exportion consult-grep to wgrep, file to wdeired, and consult-localtion to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (pcase-let ((`(,type . ,candidates)
                 (run-hook-with-args-until-success 'embark-candidate-collectors)))
      (pcase type
        ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                         (embark-export)))
        ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
                 (embark-export)))
        ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                             (embark-export)))
        (x (user-error "embark category %S doesn't support writeable export" x)))))

  (eval-after-load 'consult
    '(eval-after-load 'embark
       '(progn
          (require 'embark-consult)
          (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))
  :bind
  (:map minibuffer-local-map
        ("C-c C-e" . embark-export-write)))

(use-package wgrep
  :hook (grep-setup . wgrep-setup)
  :config
  (setq wgrep-change-readonly-file t))

;; Rich annotations in the minibuffer
(use-package marginalia
  :hook (after-init . marginalia-mode))

;;
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'init-completion)
;;; init-completion.el ends here
