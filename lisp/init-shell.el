;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun shell-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(defun shell-self-destroy-sentinel (proc _exit-msg)
  "Make PROC self destroyable."
  (when (memq (process-status proc) '(exit signal stop))
    (kill-buffer (process-buffer proc))
    (ignore-errors (delete-window))))

(defun shell-delete-window (&optional win)
  "Delete WIN wrapper."
  (ignore-errors (delete-window win)))

;; General term mode
;; If you use bash, directory track is supported natively.
;; See https://www.emacswiki.org/emacs/AnsiTermHints for more information.
(use-package term
  :straight (:type built-in)
  :hook ((term-mode . shell-mode-common-init)
         (term-mode . term-mode-prompt-regexp-setup)
         (term-exec . term-mode-set-sentinel))
  :general
  (thomas-leader
    "ot" 'term)
  :config
  (defun term-mode-prompt-regexp-setup ()
    "Setup `term-prompt-regexp' for term-mode."
    (setq-local term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

  (defun term-mode-set-sentinel ()
    "Close buffer after exit."
    (when-let ((proc (ignore-errors (get-buffer-process (current-buffer)))))
      (set-process-sentinel proc #'shell-self-destroy-sentinel))))

;; The Emacs shell & friends
(use-package eshell
  :straight (:type built-in)
  :hook (eshell-mode . shell-mode-common-init)
  :init
  (setq eshell-directory-name (concat thomas-data-dir "eshell/"))
  :config
  ;; Prevent accident typing
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)
  (defalias 'eshell/nvim 'find-file)

  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (font-lock-ensure)))
      (buffer-string)))

  (defun eshell/f (filename &optional dir)
    "Search for files matching FILENAME in either DIR or the
current directory."
    (let ((cmd (concat
                (executable-find "find")
                " " (or dir ".")
                "      -not -path '*/.git*'"
                " -and -not -path 'build'"    ;; the cmake build directory
                " -and"
                " -type f"
                " -and"
                " -iname '*" (format "%s" filename) "*'")))
      (eshell-command-result cmd)))

  (defun eshell/z ()
    "cd to directory with completions."
    (let ((dir (completing-read "Directory: " (delete-dups (ring-elements eshell-last-dir-ring)) nil t)))
      (eshell/cd dir)))

  (defun eshell/bd ()
    "cd to parent directory with completions."
    (let ((dir default-directory)
          dirs)
      (while (not (string-empty-p dir))
        (push (file-name-directory dir) dirs)
        (setq dir (substring dir 0 -1)))
      (let ((dir (completing-read "Directory: " dirs nil t)))
        (eshell/cd dir))))
  :config
  ;; The following cmds will run on term.
  (setq eshell-visual-commands '("top" "htop" "less" "more" "telnet")
        eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show"))
        eshell-visual-options '(("git" "--help" "--paginate")))
  ;; Completion like bash
  (setq eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions nil)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

(use-package em-rebind
  :straight (:type built-in)
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :straight (:type built-in)
  :bind (:map eshell-mode-map
              ([remap kill-region] . backward-kill-word)
              ([remap delete-char] . eshell-delchar-or-maybe-eof))
  :config
  ;; Delete the last "word"
  (dolist (ch '(?_ ?- ?.))
    (modify-syntax-entry ch "w" eshell-mode-syntax-table)))

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(use-package shell
  :straight (:type built-in)
  :hook ((shell-mode . shell-mode-common-init)
         (shell-mode . revert-tab-width-to-default))
  :bind ("M-`" . shell-toggle) ;; was `tmm-menubar'
  :config
  (defun shell-toggle ()
    "Toggle a persistent shell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (if-let ((win (get-buffer-window "*shell-popup*")))
        (if (eq (selected-window) win)
            ;; If users attempt to delete the sole ordinary window, silence it.
            (shell-delete-window)
          (select-window win))
      (let ((display-comint-buffer-action '(display-buffer-at-bottom
                                            (inhibit-same-window . nil))))
        (when-let ((proc (ignore-errors (get-buffer-process (shell "*shell-popup*")))))
          (set-process-sentinel proc #'shell-self-destroy-sentinel)))))

  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8))
  :config
  (setq shell-kill-buffer-on-exit t
        shell-get-old-input-include-continuation-lines t)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

(provide 'init-shell)
;;; init-shell.el ends here
