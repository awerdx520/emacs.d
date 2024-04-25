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

(setq use-file-dialog nil ;; Suppress GUI features and more
      use-dialog-box nil

      ;; 关闭相关设置
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t

      ;; Pixelwise resize
      window-resize-pixelwise t
      frame-resize-pixelwise t
      ;; Linux specific
      x-gtk-use-system-tooltips nil
      x-underline-at-descent-line t
      ;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
      ;; for the key passphrase.
      epg-pinentry-mode 'loopback
      ;;
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
      read-char-choice-use-read-key t)

;; Show line/column number and more
(use-package simple
  :straight (:type built-in)
  :config
  (setq line-number-mode t ;; show line/column/filesize in modeline
        column-number-mode t
        size-indication-mode t
        ;; open brackets auto complete
        electric-pair-mode t
        ;; No visual feedback on copy/delete.
        copy-region-blink-delay 0
        delete-pair-blink-delay 0
        ;; confusing if no fringes (GUI only).
        visual-line-fringe-indicators '(nil right-curly-arrow)
        ;; don't save current clipboard text before replacing it
        save-interprogram-paste-before-kill nil
        ;; eliminate duplicates
        kill-do-not-save-duplicates t
        ;; include '\n' when point starts at the beginning-of-line
        kill-whole-line t
        ;; show cwd when `shell-command' and `async-shell-command'
        shell-command-prompt-show-cwd t
        ;; show the name of character in `what-cursor-position'
        what-cursor-show-names t
        ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
        read-extended-command-predicate #'command-completion-default-include-p))

;; By default, Emacs stores `authinfo' in $HOME and in plain-text. Let's not do
;; that, mkay? This file stores usernames, passwords, and other treasures for
;; the aspiring malicious third party. You'll need a GPG setup though.
(setq auth-sources (list (file-name-concat thomas-state-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

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

;; Buffer index
(use-package imenu
  :straight (:type built-in)
  :hook (imenu-after-jump . recenter))

;; Gerneral Keybinding
(use-package general
  :demand t
  :config
  ;; leader
  (general-create-definer thomas-leader
    :states '(normal visual motion)
    :prefix thomas-leader-key
    :non-normal-prefix thomas-leader-alt-key
    :keymaps 'override)
  ;; localleader
  (general-create-definer thomas-localleader
    :states '(normal visual motion)
    :prefix thomas-localleader-key
    :non-normal-prefix (concat thomas-leader-alt-key " m")
    :keymaps 'override)

  ;; buffer
  (general-create-definer thomas-leader-buffer
    :states '(normal visual motion)
    :prefix "SPC b"
    :non-normal-prefix (concat thomas-leader-alt-key " b")
    :keymaps 'override
    "" '(:ignore t :wk "buffer"))
  ;; code
  (general-create-definer thomas-leader-code
    :states '(normal visual motion)
    :prefix "SPC c"
    :non-normal-prefix (concat thomas-leader-alt-key " c")
    :keymaps 'override
    "" '(:ignore t :wk "code"))

  ;; file
  (general-create-definer thomas-leader-file
    :states '(normal visual motion)
    :prefix "SPC f"
    :non-normal-prefix (concat thomas-leader-alt-key " f")
    :keymaps 'override
    "" '(:ignore t :wk "file"))
  ;; git
  (general-create-definer thomas-leader-git
    :states '(normal visual motion)
    :prefix "SPC g"
    :non-normal-prefix (concat thomas-leader-alt-key " h")
    :keymaps 'override
    "" '(:ignore t :wk "git"))
  ;; help
  (general-create-definer thomas-leader-help
    :states '(normal visual motion)
    :prefix "SPC h"
    :non-normal-prefix (concat thomas-leader-alt-key " h")
    :keymaps 'override
    "" '(:ignore t :wk "insert"))
  ;; open
  (general-create-definer thomas-leader-open
    :states '(normal visual motion)
    :prefix "SPC o"
    :non-normal-prefix (concat thomas-leader-alt-key " o")
    :keymaps 'override
    "" '(:ignore t :wk "open"))

  ;;project
  (general-create-definer thomas-leader-project
    :states '(normal visual motion)
    :prefix "SPC p"
    :non-normal-prefix (concat thomas-leader-alt-key " p")
    :keymaps 'override
    "" '(:ignore t :wk "project"))

  ;; quit
  (general-create-definer thomas-leader-quit
    :states '(normal visual motion)
    :prefix "SPC q"
    :non-normal-prefix (concat thomas-leader-alt-key " q")
    :keymaps 'override
    "" '(:ignore t :wk "quit/session"))

  ;; insert
  (general-create-definer thomas-leader-insert
    :states '(normal visual motion)
    :prefix "SPC i"
    :non-normal-prefix (concat thomas-leader-alt-key " i")
    :keymaps 'override)

  ;; remote
  (general-create-definer thomas-leader-remote
    :states '(normal visual motion)
    :prefix "SPC r"
    :non-normal-prefix (concat thomas-leader-alt-key " r")
    :keymaps 'override)

  ;; search
  (general-create-definer thomas-leader-search
    :states '(normal visual motion)
    :prefix "SPC s"
    :keymaps 'override
    "" '(:ignore t :wk "search"))

  ;; window
  (general-create-definer thomas-leader-window
    :states '(normal visual motion)
    :prefix "SPC w"
    :non-normal-prefix (concat thomas-leader-alt-key " w")
    :keymaps 'override
    "" '(:ignore t :wk "window"))

  (thomas-leader
   "RET" 'consult-bookmark
   "'" 'vertico-repeat
   "." 'consult-find
   ":" 'execute-extended-command
   ";" 'pp-eval-expression
   "`" 'switch-to-buffer
   "a" '(embark-act :wk "Actions"))

  (thomas-leader-buffer
   "-" 'thomas/toggle-narrow-buffer
   "[" 'previous-buffer
   "]" 'next-buffer
   "b" 'consult-buffer
   "c" 'clone-indirect-buffer
   "C" 'clone-indirect-buffer-other-window
   "d" 'kill-current-buffer
   "i" 'ibuffer
   "k" 'kill-current-buffer
   ;;"K" 'thomas/kill-all-buffer
   "l" 'evil-switch-to-windows-last-buffer
   "m" 'bookmark-set
   "M" 'bookmark-delete
   "n" 'next-buffer
   "N" 'evil-buffer-new
   "p" 'previous-buffer
   "r" 'revert-buffer
   "s" 'save-buffer
   "S" 'evil-write-all
   "y" '+default/yank-buffer-contents
   "z" 'bury-buffer)

  (thomas-leader-code
   "c" 'compile
   "C" 'recompile
   "d" 'xref-find-definitions
   "D" 'xref-find-references)

  (thomas-leader-file
   "C" 'thomas/copy-this-file
   "d" '+default/dired
   "f" 'find-file)

  (thomas-leader-insert
   "e" 'emoji-search)

  (thomas-leader-open
   "-" 'dired-jump
   "A" 'org-agenda
   "f" 'make-frame
   "F" 'select-frame-by-name
   "t" 'eshell)

  (thomas-leader-quit
   "f" 'thomas/delete/frame-with-prompt
   "K" 'save-buffers-kill-emacs
   "q" 'save-buffers-kill-terminal)

  (thomas-leader-search
   "b" '+default/search-buffer
   "B" '(cmd!! #'consult-line-multi 'all-buffers)
   "f" 'consult-locate
   "i" 'consult-imenu
   "I" 'consult-imenu-multi
   "m" 'consult-bookmark
   "r" 'consult-mark)

  (thomas-leader-window
   "=" 'balance-windows
   "f" 'ffap-other-window
   "T" 'tear-off-window
   "C-o" 'delete-other-windows))

;; Tips for next keystroke
(use-package which-key
  :hook (after-init . which-key-mode)
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

(provide 'init-base)
;;; init-base.el ends here
