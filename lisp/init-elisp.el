;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package elisp-mode
  :straight (:type built-in)
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . rainbow-delimiters-mode)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-c" . eval-to-comment)
         :map lisp-interaction-mode-map
         ("C-c C-c" . eval-to-comment))
  :config
  (defconst eval-as-comment-prefix ";;=> ")

  ;; Imitate scala-mode
  ;; from https://github.com/dakra/dmacs
  (defun eval-to-comment (&optional arg)
    (interactive "P")
    (let ((start (point)))
      (eval-print-last-sexp arg)
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (insert eval-as-comment-prefix)))))

;; Interactive macor expandor
(use-package macrostep)

;; Elisp REPL mode, Use ielm open
(use-package ielm
  :defer t
  :config
  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  (setq ielm-font-lock-keywords
        (append '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
                   (1 font-lock-comment-face)
                   (2 font-lock-constant-face)))
                (when (require 'highlight-numbers nil t)
                  (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode))
                (cl-loop for (matcher . match-highlights)
                         in (append lisp-el-font-lock-keywords-2
                                    lisp-cl-font-lock-keywords-2)
                         collect
                         `((lambda (limit)
                             (when ,(if (symbolp matcher)
                                        `(,matcher limit)
                                      `(re-search-forward ,matcher limit t))
                               ;; Only highlight matches after the prompt
                               (> (match-beginning 0) (car comint-last-prompt))
                               ;; Make sure we're not in a comment or string
                               (let ((state (syntax-ppss)))
                                 (not (or (nth 3 state)
                                          (nth 4 state))))))
                           ,@match-highlights)))))

(use-package flycheck-cask
  :defer t
  :hook (elisp-mode . (lambda ()
                        (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t))))

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

(use-package elisp-demos
  :defer t
  :init
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Elisp Unittest
(use-package buttercup
  :defer t
  :minor ("/test[/-].+\\.el$" . buttercup-minor-mode)
  :hook (bufferp-minor-mode . evil-normalize-keymaps)
  :preface
  ;; buttercup.el doesn't define a keymap for `buttercup-minor-mode', as we have
  ;; to fool its internal `define-minor-mode' call into thinking one exists, so
  ;; it will associate it with the mode.
  (defvar buttercup-minor-mode-map (make-sparse-keymap)))


(use-package highlight-quoted
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . highlight-quoted-mode))

(provide 'init-elisp)

;;; init-elisp.el ends here
