;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;; Optimize for very long lines
(setq-default bidi-paragraph-direction 'left-to-right
              fill-column 80
              ;; No tabs
              indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil)

;; By default, Emacs stores `authinfo' in $HOME and in plain-text. Let's not do
;; that, mkay? This file stores usernames, passwords, and other treasures for
;; the aspiring malicious third party. You'll need a GPG setup though.
(setq auth-sources (list (file-name-concat thomas-state-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list" thomas-cache-dir) t)))

(setq use-dialog-box nil
      ;; 关闭相关设置
      inhibit-startup-screen t
      inhibit-startup-message t

      ;; Pixelwise resize
      window-resize-pixelwise t
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

      ;; Cutting and pasting use primary/clipboard
      select-enable-primary t
      select-enable-clipboard t
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
      ;; Sane defaults
      use-short-answers t
      ;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
      y-or-n-p-use-read-key t
      kill-do-not-save-duplicates t
      read-char-choice-use-read-key t)

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

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; auto-revert
(use-package auto-revert
  :straight (:type built-in)
  :diminish  auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; This package lets you enable minor modes based on file name and contents.
;; To find the right modes, it checks filenames against patterns in `auto-minor-mode-alist'
;; and file contents against `auto-minor-mode-magic-alist'.
;; These work like the built-in Emacs variables `auto-mode-alist' and `magic-mode-alist'.
;; Unlike major modes, all matching minor modes are enabled, not only the first match.
(use-package auto-minor-mode
  :config
  (nconc
   auto-mode-alist
   '(("/LICENSE\\'" . text-mode)
     ("\\.log\\'" . text-mode)
     ("rc\\'" . conf-mode)
     ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode))))

;; Gerneral Keybinding
(use-package general
  :demand t
  :config
  ;; leader
  (general-create-definer thomas-leader
    :states '(normal visual motion)
    :prefix thomas-leader-key
    :non-normal-prefix thomas-leader-alt-key
    :keymaps 'override
    ;; Top
    ":" 'execute-extended-command
    ";" 'pp-eval-expression
    "`" 'switch-to-buffer

    ;; Buffer
    "b" '(:ignore t :wk "buffer")
    ;;    "b-" 'thomas/toggle-narrow-buffer
    "b[" 'previous-buffer
    "b]" 'next-buffer
    "bc" 'clone-indirect-buffer
    "bC" 'clone-indirect-buffer-other-window
    "bd" 'kill-current-buffer
    "bi" 'ibuffer
    "bk" 'kill-current-buffer
    "bl" 'evil-switch-to-windows-last-buffer
    "bm" 'bookmark-set
    "bM" 'bookmark-delete
    "bn" 'next-buffer
    "bN" 'evil-buffer-new
    "bp" 'previous-buffer
    "br" 'revert-buffer
    "bs" 'save-buffer
    "bS" 'evil-write-all
    "by" '+default/yank-buffer-contents
    "bz" 'bury-buffer

    ;; Code
    "c" '(:ignore t :wk "code")
    "cc" 'compile
    "cC" 'recompile

    ;; File
    "f" '(:ignore t :wk "file")
    "ff" 'find-file
    "fe" '+default/find-file-in-emacs
    ;; Git
    "g" '(:ignore t :wk "git")

    ;; Help
    "h" '(:ignore t :wk "help")

    ;; Insert
    "i" '(:ignore t :wk "insert")
    "ie" 'emoji-search

    ;; Open
    "o" '(:ignore t :wk "open")
    "o-" 'dired-jump
    "oA" 'org-agenda
    "of" 'make-frame
    "oF" 'select-frame-by-name

    ;; Project
    "p" '(:ignore t :wk "project")

    ;; Quit
    "q" '(:ignore t :wk "quit/session")
    "qK" 'save-buffers-kill-emacs
    "qq" 'save-buffers-kill-terminal

    ;; Remote
    "r" '(:ignore t :wk "remote")

    ;; Search
    "s" '(:ignore t :wk "search")
    "se" '+default/search-emacs
    "sb" '+default/search-buffer
    "ss" '+default/search-buffer
    "sB" '+default/search-all-buffer

    ;; Window
    "w" '(:ignore t :wk "window")
    "w=" 'balance-windows
    "wf" 'ffap-other-window
    "wT" 'tear-off-window
    "w C-o" 'delete-other-windows))

;; Tips for next keystroke
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :general
  (thomas-leader
   ;;
   "hbf" 'which-key-show-full-keymap
   "hbi" 'which-key-show-minor-mode-keymap
   "hbk" 'which-key-show-keymap
   "hbm" 'which-key-show-major-mode
   "hbt" 'which-key-show-top-level)
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  (add-hook 'which-key-init-buffer-hook
            (lambda () (setq line-spacing 3)))

  (which-key-add-key-based-replacements thomas-leader-key "<leader>")
  (which-key-add-key-based-replacements thomas-localleader-key "<localleader>"))


(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config
  ;; Ensure babel's execution environment matches the host buffer's.
  (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment))

(provide 'init-base)
;;; init-base.el ends here
