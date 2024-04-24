;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Compilation Mode
(use-package compile
  :straight (:type built-in)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-always-kill t
        compilation-scroll-output t)
  ;; Save all buffers on M-x `compile'
  (setq compilation-ask-about-save nil))

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

;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :config
  (setq quickrun-focus-p nil
        quickrun-input-file-extension ".qr"))

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

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :bind (("M-g j" . dumb-jump-go)
         ("M-g J" . dumb-jump-go-other-window))
  :config
  (setq dumb-jump-quiet t
        dumb-jump-aggressive t
        dumb-jump-selector 'completing-read))

;; A fancy ctags frontend
(use-package citre
  :init
  ;; Load the prelude.
  (require 'citre-config)
  :bind (("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back)
         ("C-c c p" . citre-peek)
         ("C-c c a" . citre-ace-peek)
         ("C-c c u" . citre-update-this-tags-file))
  :config
  (setq citre-enable-capf-integration nil
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode-modes '(prog-mode)))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :straight (:type built-in)
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                              hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  (setq hs-set-up-overlay #'hideshow-folded-overlay-fn))

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


(use-package lsp-bridge
  :straight '(lsp-bridge  :fetcher github :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :config
  (setq lsp-bridge-enable-log nil))

(provide 'init-develop)

;;; init-develop.el ends here
