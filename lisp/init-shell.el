;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package vterm
  :hook (vterm-mode . hide-mode-line-mode)
  :general
  (thomas-leader-define "ot" 'vterm)
  (:keymaps 'vterm-mode-map "C-q" 'vterm-send-next-key)
  :preface
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :config
  ;; TODO Popup
  ;;

  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)

  (defun +thomas/set-vterm-settings ( )
    (setq confirm-kill-processes nil ; Don't prompt about dying processes when killing vterm
          ;; Prevent premature horizontal scrolling
          hscroll-margin 0))
  (add-hook 'vterm-mode-hook #'+thomas/set-vterm-settings))

;; The Emacs shell & friends
;; TODO 学习 Eshell
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

(provide 'init-shell)
;;; init-shell.el ends here
