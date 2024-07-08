;;; init-project.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defvar project-ignore-directory-list '("/home/thomas" "/" "/var" "/proc" "/usr")
  "忽略指定目录作为项目目录.")

(defvar project-flag-list '((".project")
                            ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                            ("Makefile" "README.org" "README.md" "Readme.org" "Readme.md")))

;;
(use-package project
  :straight (:type built-in)
  :custom (project-list-file (concat thomas-data-dir "projects"))
  :config
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun project-try-local (dir)
    "Determine if DIR is a non-Git project."
    (unless (member (expand-file-name "" dir) project-ignore-directory-list)
      (catch 'ret
        (let ((pr-flags project-flag-list))
          (dolist (current-level pr-flags)
            (dolist (f current-level)
              (when-let ((root (locate-dominating-file dir f)))
                (when (not (member (expand-file-name "" root) project-ignore-directory-list))
                  (throw 'ret (cons 'local root))))))))))

  (setq project-find-functions '(project-try-local project-try-vc)))

(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :after project
  :config
  ;; Require projections immediately after project.el.
  (with-eval-after-load 'project
    (require 'projection))

  ;; Uncomment if you want to disable prompts for compile commands customized in .dir-locals.el
  (put 'projection-commands-configure-project 'safe-local-variable #'stringp)
  (put 'projection-commands-build-project 'safe-local-variable #'stringp)
  (put 'projection-commands-test-project 'safe-local-variable #'stringp)
  (put 'projection-commands-run-project 'safe-local-variable #'stringp)
  (put 'projection-commands-package-project 'safe-local-variable #'stringp)
  (put 'projection-commands-install-project 'safe-local-variable #'stringp))

(use-package projection-multi
  ;; Allow interactively selecting available compilation targets from the current
  ;; project type.
  ;; :init
  ;; (define-key global-map "RET" #'projection-multi-compile)
  )

(use-package projection-multi-embark
  :after embark
  :after projection-multi
  :demand t
  ;; Add the projection set-command bindings to `compile-multi-embark-command-map'.
  :config (projection-multi-embark-setup-command-map))

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
