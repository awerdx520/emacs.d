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

;; Startup maximum
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Font size
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  (cl-loop for font in '("Cascadia Code" "SF Mono" "Source Code Pro"
                         "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                         "Lucida Console" "Consolas" "SAS Monospace")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                    :weight 'normal
                                    :slant 'normal
                                    :size (cond ((eq system-type 'gnu/linux) 13.0)
                                                ((eq system-type 'windows-nt) 12.5)))))
  (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                         "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode
                                    (font-spec :family font
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0)))
                                    nil 'prepend))
  (cl-loop for font in '("思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
                         "Source Han Sans CN" "Source Han Serif CN"
                         "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                         "Microsoft Yahei UI" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0)))))
  (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p font)
           return (set-fontset-font t '(#x20000 . #x2A6DF)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0))))))
;;
;;; General UX
;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;;
;;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Thomas Emacs")
      icon-title-format frame-title-format)


;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUIs are inconsistent across systems, desktop environments, and themes,
;;   and don't match the look of Emacs. They also impose inconsistent shortcut
;;   key paradigms. I'd rather Emacs be responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)

(with-eval-after-load 'comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048)) ; double the default

(with-eval-after-load 'compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(use-package hl-line
  ;; Highlights the current line
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
  :when (display-graphic-p)
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline))


;; Use Iosvkem in terminals
(use-package doom-themes
  :config
  (doom-themes-org-config)
  (let ((theme (if (display-graphic-p)
                   'doom-one
                 'doom-Iosvkem)))
    (load-theme theme t)))

(use-package paren
  ;; highlight matching delimiters
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package rainbow-delimiters
  :config
  ;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
  ;; languages like Lisp. I reduce it from it's default of 9 to reduce the
  ;; complexity of the font-lock keyword and hopefully buy us a few ms of
  ;; performance.
  (setq rainbow-delimiters-max-face-count 4))

;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Highlight TODO
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t i" . hl-todo-insert)
              ("C-c t o" . hl-todo-occur)
              ("C-c t s" . hl-todo-rgrep)))

(use-package transient
  :straight (:type built-in)
  :bind (("C-c h o" . scroll-other-window-menu)
         ("C-c h t" . background-opacity-menu))
  :init
  (setq transient-levels-file (concat thomas-data-dir "transient/levels")
        transient-values-file (concat thomas-data-dir "transient/values")
        transient-history-file (concat thomas-data-dir "transient/history"))
  :config
  (transient-define-prefix scroll-other-window-menu ()
    "Scroll other window."
    :transient-suffix     'transient--do-stay
    [["Line"
      ("j" "next line" scroll-other-window-line)
      ("k" "previous line" scroll-other-window-down-line)]
     ["Page"
      ("C-f" "next page" scroll-other-window)
      ("C-b" "previous page" scroll-other-window-down)]])

  (defun scroll-other-window-line ()
    "Scroll up of one line in other window."
    (interactive)
    (scroll-other-window 1))

  (defun scroll-other-window-down-line ()
    "Scroll down of one line in other window."
    (interactive)
    (scroll-other-window-down 1))

  (transient-define-prefix background-opacity-menu ()
    "Set frame background opacity."
    [:description
     background-opacity-get-alpha-str
     ("+" "increase" background-opacity-inc-alpha :transient t)
     ("-" "decrease" background-opacity-dec-alpha :transient t)
     ("=" "set to ?" background-opacity-set-alpha)])

  (defun background-opacity-inc-alpha (&optional n)
    (interactive)
    (let* ((alpha (background-opacity-get-alpha))
           (next-alpha (cl-incf alpha (or n 1))))
      (set-frame-parameter nil 'alpha-background next-alpha)))

  (defun background-opacity-dec-alpha ()
    (interactive)
    (background-opacity-inc-alpha -1))

  (defun background-opacity-set-alpha (alpha)
    (interactive "nSet to: ")
    (set-frame-parameter nil 'alpha-background alpha))

  (defun background-opacity-get-alpha ()
    (pcase (frame-parameter nil 'alpha-background)
      ((pred (not numberp)) 100)
      (`,alpha alpha)))

  (defun background-opacity-get-alpha-str ()
    (format "Alpha %s%%" (background-opacity-get-alpha))))

(provide 'init-ui)
;;; init-ui.el ends here
