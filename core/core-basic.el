;;; core-basic.el 基础配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: 五月 07, 2024
;; Modified: 五月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/core-basic
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

;; Optimize for very long lines
(setq-default fill-column 80
              tab-width 4
              indent-tabs-mode nil ; No tabs
              tab-always-indent nil)


;; 简化 yes-or-no 输入
(setq use-short-answers t
      ;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
      y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)


;; 禁用对话框
(setq use-dialog-box nil)
;; 关闭启动信息和屏幕
(setq inhibit-startup-screen t
      inhibit-startup-message t)
;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t
      ;; Linux specific
      x-gtk-use-system-tooltips t
      x-underline-at-descent-line t
      ;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
      ;; for the key passphrase.
      epg-pinentry-mode 'loopback
      bidi-inhibit-bpa t
      ;; Always load the newest file
      load-prefer-newer t
      ;; No lock files
      create-lockfiles nil

      ;; No gc for font caches
      inhibit-compacting-font-caches t
      ;; Improve display
      display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t
      ;; No annoying bell
      ring-bell-function 'ignore
      ;; No eyes distraction
      blink-cursor-mode nil
      ;; Smooth scroll & friends
      scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position 'always
      ;; The nano style for truncated long lines.
      auto-hscroll-mode 'current-line
      ;; Disable auto vertical scroll for tall lines
      auto-window-vscroll nil
      ;; Dont move points out of eyes
      mouse-yank-at-point t
      kill-do-not-save-duplicates t)


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

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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
      indicate-empty-lines nil
      left-margin-width 2
      right-margin-width 2)

;; 设置边缘 finge 宽度
(when (display-graphic-p)
  (set-fringe-mode 2))

;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Thomas Emacs")
      icon-title-format frame-title-format)

;;分屏的时候使用上下分屏
;;(setq split-width-threshold nil)
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



;; By default, Emacs stores `authinfo' in $HOME and in plain-text. Let's not do
;; that, mkay? This file stores usernames, passwords, and other treasures for
;; the aspiring malicious third party. You'll need a GPG setup though.
(setq auth-sources (list (file-name-concat thomas-state-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list" thomas-cache-dir) t)))

;; Startup Maximum
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)

(provide 'core-basic)
;;; core-basic.el ends here
