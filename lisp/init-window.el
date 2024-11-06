;;; init-window.el Emacs 窗口相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-window
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package winner
  :straight (:type built-in)
  ;; undo/redo changes to Emacs' window layout
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (after-init . winner-mode)
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))

;; 跳转 window
(use-package ace-window
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :custom-face
  (aw-leading-char-face ((t (:foreground "white" :background "#2a6041"
                                         :weight bold :height 2.5 :box (:line-width 10 :color "#2a6041")))))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always nil
        aw-ignore-current t
        aw-ignore-on t
        aw-scope 'frame
        aw-background t))

;; TODO 窗口弹出规则
;; (use-package shackle
;;   :hook (after-init . shackle-mode)
;;   :init
;;   (setq shackle-default-size 0.35
;;         shackle-default-alignment 'below)

;;   (setq shackle-rules '((magit-diff-mode :align 'below :size 0.6)
;;                         ("*Messages*" :align 'below :size 0.4)
;;                         (term-mode :align 'below :size 0.4))))


;; ;; 弹出窗口管理
;; (use-package popper
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*" ;; Emacs 信息窗口
;;           ;; 抑制所有以 Output* 结尾的缓冲区和完成缓冲区，可以通过 popoer-toggle
;;           ("Output\\*$" . hide)
;;           "\\*Pp Eval Output\\*$"
;;           "\\*Compile-Log\\*"
;;           "\\*Completions\\*"
;;           "\\*Warnings\\*"
;;           "\\*Flymake diagnostics.*\\*"
;;           "\\*Async Shell Command\\*"
;;           "\\*Apropos\\*"
;;           "\\*Backtrace\\*"
;;           "\\*prodigy\\*"
;;           "\\*Calendar\\*"
;;           "\\*Embark Actions\\*"
;;           "\\*Finder\\*"
;;           "\\*Kill Ring\\*"
;;           "\\*Embark Export:.*\\*"
;;           "\\*Edit Annotation.*\\*"
;;           bookmark-bmenu-mode
;;           lsp-bridge-ref-mode
;;           comint-mode
;;           compilation-mode
;;           help-mode helpful-mode
;;           tabulated-list-mode
;;           Buffer-menu-mode
;;           occur-mode
;;           gnus-article-mode devdocs-mode
;;           grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
;;           process-menu-mode list-environment-mode cargo-process-mode
;;           "^\\*gt-result\\*$" ; 翻译

;;           ;; 从主模式 fundamental-mode 派生的任何少于 10 行的缓冲区都将被视为弹出窗口。
;;           (lambda (buf) (with-current-buffer buf
;;                           (and (derived-mode-p 'fundamental-mode)
;;                                (< (count-lines (point-min) (point-max))
;;                                   10))))

;;           ;;         "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
;;           ;;          rustic-cargo-outdated-mode rustic-cargo-test-moed

;;           "^\\*eshell.*\\*.*$" eshell-mode
;;           "^\\*shell.*\\*.*$"  shell-mode
;;           "^\\*terminal.*\\*.*$" term-mode
;;           "^\\*vterm.*\\*.*$"  vterm-mode

;;           "\\*ELP Profiling Restuls\\*" profiler-report-mode
;;           "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
;;           "\\*[Wo]*Man.*\\*$"
;;           "\\*ert\\*$" overseer-buffer-mode
;;           "\\*quickrun\\*$"
;;           "\\*vc-.*\\*$"
;;           "\\*eldoc\\*"
;;           "^\\*macro expansion\\**"

;;           "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
;;           "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode ; golang
;;           "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*" ; docker
;;           ))


;;   (setq popper-echo-dispatch-actions t
;;         ;; group by project.el  project root, with  fall back to  default-directory
;;         popper-group-function #'popper-group-by-directory

;;         ;; 关闭 popper 控制弹出窗口位置, 使用 shackle 包控制
;;         popper-display-control nil)

;;   ;; 自适应弹出窗口高度
;;   (defun +popper-fit-window-height (win)
;;     "Determine the height of popup window WIN by fitting it to the buffer's content."
;;     (fit-window-to-buffer win (floor (frame-height) 3)
;;                           (floor (frame-height) 3)))

;;   (setq popper-window-height #'+popper-fit-window-height)

;;   ;; 开启 popper-mode
;;   (popper-mode +1)
;;   (popper-echo-mode +1)
;;   :config
;;   ;;
;;   (defun +popper-close-window-hack (&rest _)
;;     "Close popper window via `C-g'."
;;     ;; `C-g' can deactivate region
;;     (when (and (called-interactively-p 'interactive)
;;                (not (region-active-p))
;;                popper-open-popup-alist)
;;       (let ((window (caar popper-open-popup-alist)))
;;         (when (window-live-p window)
;;           (delete-window window)))))
;;   (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(provide 'init-window)
;;; init-window.el ends here
