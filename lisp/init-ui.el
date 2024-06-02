;;; init-ui.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-ui
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'core-const)

(use-package doom-themes
  :demand t
  ;; 包含大量漂亮的主题，
  :hook ((thomas-load-theme . doom-themes-org-config)
         (thomas-load-theme . doom-themes-visual-bell-config))
  :config
  (let ((theme (if (display-graphic-p)
                   'doom-vibrant
                 'doom-one-light)))
    (load-theme theme t)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type 0)

  :config
  ;; anzu.el 提供了一个次要模式，可以在各种搜索模式下在模式行中显示当前匹配和总匹配信息。
  (use-package anzu)
  ;; anzu for evil-mode
  (use-package evil-anzu
    :after evil
    :config (global-anzu-mode +1)))


(use-package dashboard
  :demand t
  :custom-face
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-heading-face ((t (:weight bold))))
  :general
  (:states 'normal :keymaps 'dashboard-mode-map
           "TAB" 'widget-forward
           "RET" 'widget-button-press
           "g" 'dashboard-refresh-buffer
           "}" 'dashboard-next-section
           "{" 'dashboard-previous-section)
  :config
  ;; 在 Server 模式下，创建 frame 时显示仪表盘
  (setq initial-buffer-choice (lambda ()
                                (get-buffer-create "*dashboard*"))
        ;;
        dashboard-startup-banner '2
        dashboard-projects-backend 'project-el
        ;;
        dashboard-set-init-info t
        dashboard-set-navigator t
        ;;
        dashboard-page-separator "\n\f\n")
  ;;
  (setq dashboard-items '((recents   . 5)
                          (projects  . 7)
                          (agenda . 5)
                          (bookmarks . 5)))

  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-icon-type 'nerd-icons
        dashboard-heading-icons '((agenda . "nf-oct-calendar")
                                  (recents . "nf-oct-file")
                                  (projects . "nf-oct-project")
                                  (bookmarks . "nf-oct-bookmark")))
  ;; 启动 dashboard 设置
  (dashboard-setup-startup-hook))



(use-package page-break-lines
  :hook (dashboard-mode . page-break-lines-mode))


(use-package hide-mode-line
  ;; Hide the mode line in completion popups and MAN pages because they serve
  ;; little purpose there, and is better hidden.
  :hook (completion-list-mode Man-mode))

;;
;;; 一些增强包
(use-package hl-line
  :straight (:type built-in)
  ;; 高亮当前行
  :hook (after-init . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
      org-agenda-mode dired-mode)
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar thomas--hl-line-mode nil)
  (defun thomas-truly-disable-hl-line-h ()
    (unless hl-line-mode
      (setq-local thomas--hl-line-mode nil)))
  (add-hook 'hl-line-mode-hook #'thomas-truly-disable-hl-line-h)

  (defun thomas-disable-hl-line-h ()
    (when hl-line-mode
      (hl-line-mode -1)
      (setq-local thomas--hl-line-mode t)))
  (add-hook 'evil-visual-state-entry-hook  #'thomas-disable-hl-line-h)
  (add-hook 'activate-mark-hook #'thomas-disable-hl-line-h)

  (defun thomas-enable-hl-line-maybe-h ()
    (when thomas--hl-line-mode
      (hl-line-mode +1)))
  (add-hook 'evil-visual-state-exit-hook  #'thomas-enable-hl-line-maybe-h)
  (add-hook 'deactivate-mark-hook #'thomas-enable-hl-line-maybe-h))

(use-package nerd-icons
  ;; A Library for Nerd Font icons
  :config
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (when (display-graphic-p)
    (setq nerd-icons-font-family "Symbols Nerd Font Mono")))


(use-package paren
  ;; 高亮显示匹配的括号
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package whitespace
  ;; 空白字符配置
  :config
  (setq whitespace-line-column nil
        whitespace-style
        '(face indentation tabs tab-mark spaces space-mark newline newline-mark
          trailing lines-tail)
        whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.]))))

(use-package highlight-numbers
  ;;许多主要模式不突出显示数字文字，因此我们为它们突出显示
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp
                "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((emacs-lisp-mode conf-space-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
  ;; languages like Lisp. I reduce it from it's default of 9 to reduce the
  ;; complexity of the font-lock keyword and hopefully buy us a few ms of
  ;; performance.
  (setq rainbow-delimiters-max-face-count 4))

(use-package hl-todo
  :hook (thomas-first-file . global-hl-todo-mode)
  :general
  (:states '(normal visual)
           "]t" 'hl-todo-next
           "[t" 'hl-todo-previous)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces `(("BUG" error bold)
                                ("FIXME" error bold)
                                ("TODO" warning bold)
                                ("NOTE" success bold)
                                ("HACK" font-lock-constant-face bold)
                                ("REVIEW" font-lock-keyword-face bold)
                                ("DEPRECATED" font-lock-doc-face bold))))

(use-package symbol-overlay
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :config
  (setq symbol-overlay-scope t)
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode 'yaml-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

(provide 'init-ui)
;;; init-ui.el ends here
