;;; init-project.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar +project-ignore-project-list '("~" "/home/thomas" "/")
  "忽略指定目录作为项目目录.")

;;
(use-package project
  :straight (:type built-in)
  :custom (project-list-file (concat thomas-data-dir "projects"))
  :config
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun thomas/project-try-flag (dir)
    "Determine if DIR is a non-Git project."
    (unless (member (expand-file-name "" dir)
                    +project-ignore-project-list)
      (catch 'ret
        (let ((pr-flags '((".project")
                          ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                          ("Makefile" "README.org" "README.md"))))
          (dolist (current-level pr-flags)
            (dolist (f current-level)
              (when-let ((root (locate-dominating-file dir f)))
                (throw 'ret (cons 'local root)))))))))

  (setq project-find-functions '(project-try-vc thomas/project-try-flag)))

(use-package compile
  ;; Compilation Mode
  :straight (:type built-in)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-always-kill t ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t))

(use-package comint
  ;; ComintMode 用于制作 shell 或 repl 之类的缓冲区，在其中与外部进程交互。
  :straight (:type built-in)
  :config
  (setq comint-prompt-read-only t
        ;; double the default
        comint-buffer-maximum-size 2048)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; The unified debugger
(use-package gud
  :straight (:type built-in)
  :hook (gud-mode . gud-tooltip-mode)
  :config
  (setq gud-highlight-current-line t))

;; GDB specific config
(use-package gdb-mi
  :straight (:type built-in)
  :commands gdb
  :config
  (setq gdb-show-main t
        gdb-display-io-nopopup t
        gdb-show-changed-values t
        gdb-delete-out-of-scope t
        gdb-use-colon-colon-notation t
        gdb-debuginfod-enable-setting nil
        gdb-restore-window-configuration-after-quit t))

;; Insert SPDX license header
(use-package spdx
  :hook (prog-mode . spdx-tempo-setup)
  :config
  (setq spdx-ignore-deprecated t))

;; xref
(use-package xref
  :straight (:type built-in)
  :hook ((xref-after-return xref-after-jump) . recenter)
  :config
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (setq xref-search-program (cond ((executable-find "rg") 'ripgrep)
                                  ((executable-find "ugrep") 'ugrep)
                                  (t 'grep))
        xref-history-storage 'xref-window-local-history
        xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :config
  (setq quickrun-focus-p nil
        quickrun-input-file-extension ".qr"))

;; Browse devdocs.io
(use-package devdocs
  :custom (devdocs-data-dir (concat thomas-data-dir "devdocs"))
  :config (add-to-list 'completion-category-overrides
                       '(devdocs (styles . (flex)))))


;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :config
  (setq dumb-jump-quiet t
        dumb-jump-aggressive t
        dumb-jump-selector 'completing-read))

;; XML
(use-package nxml-mode
  :straight (:type built-in)
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode))
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

;; Config files mode
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :straight (:type built-in)
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))



(provide 'init-project)

;;; init-project.el ends here
