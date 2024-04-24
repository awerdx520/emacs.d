;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package vertico
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :config
  (setq vertico-sort-function nil))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil)
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

(use-package embark
  :bind (:map minibuffer-local-map
              ("C-;"     . embark-act)
              ("C-c C-c" . embark-export)
              ("C-c C-o" . embark-collect))
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

(use-package consult
  :bind (([remap imenu]                  . consult-imenu)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap jump-to-register]       . consult-register-load)
         ([remap point-to-register]      . consult-register-store)
         ([remap evil-show-marks]               . consult-mark)
         ([remap evil-show-registers]           . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap info-search]                   . consult-info)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-fontify-preserve nil
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))

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

  (define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write))

(use-package nerd-icons-completion
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
