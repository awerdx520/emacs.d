;;; init-editor.el 编辑相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-editor
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;
;;; Undo
(use-package undo-fu
  :hook (after-init . undo-fu-mode)
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))


(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :custom (undo-fu-session-directory (expand-file-name "undo-fu-session" thomas-cache-dir))
  :config
  (setq undo-fu-session-incompatible-files
        '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))

  (defun undo-fu-make-hashed-session-file-name-a (file)
    "HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
        filenames to prevent file paths that are too long, so we force
        `undo-fu-session--make-file-name' to use it instead of its own
        home-grown overly-long-filename generator.
        TODO PR this upstream; should be a universal issue"
    (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
              (make-backup-file-name-1 file)
              (undo-fu-session--file-name-ext)))
    (advice-add  'undo-fu-session--make-file-name
                 :override #'undo-fu-make-hashed-session-file-name-a)))

(use-package vundo
  :when (> emacs-major-version 27)
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)
  (define-key vundo-mode-map [remap doom/escape] #'vundo-quit))


;;
;;; File
;; Workaround with minified source files
(use-package so-long
  :straight (:type built-in)
  :hook (after-init . global-so-long-mode))

;; Recently opened files
(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :custom (recentf-save-file (expand-file-name  "recentf" thomas-cache-dir))
  :config
  (setq recentf-max-saved-items 300
        ;; The most sensible time to clean up your recent files list is when you quit
        ;; Emacs (unless this is a long-running daemon session).
        recentf-auto-cleanup (when (daemonp) 300))

  (defun thomas--recentf-file-truename-fn (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))

  ;; REVIEW: Use this in lieu of `doom--recentf-file-truename-fn' when we drop
  ;;   28 support. See emacs-mirror/emacs@32906819addd.
  ;; (setq recentf-show-abbreviated t)
  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'thomas--recentf-file-truename-fn)

  ;; Add dired directories to recentf file list.
  (add-hook 'dired-mode-hook (lambda () (recentf-add-file default-directory)))

  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  ;; Anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run")))))


;;
;;; Comment
(use-package newcomment
  :straight (:type built-in)
  :bind ([remap comment-dwim] . comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    "Comment or uncomment the current line or region.

If the region is active and `transient-mark-mode' is on, call
`comment-or-uncomment-region'.
Else, if the current line is empty, insert a comment and indent
it.
Else, call `comment-or-uncomment-region' on the current line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (comment-dwim nil)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  ;; `auto-fill' inside comments.
  ;;
  ;; The quoted text in `message-mode' are identified as comments, so only
  ;; quoted text can be `auto-fill'ed.
  (comment-auto-fill-only-comments t))

;;
;;; Session
;; 保存 session 相关配置，如：kill-ring 等变量
(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file (expand-file-name "savehist" thomas-cache-dir))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches

  (defun thomas-savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache size."
    (setq kill-ring
          (mapcar #'substring-no-properties
                  (cl-remove-if-not #'stringp kill-ring))
          register-alist
          (cl-loop for (reg . item) in register-alist
                   if (stringp item)
                   collect (cons reg (substring-no-properties item))
                   else collect (cons reg item))))
  (add-hook 'savehist-save-hook #'thomas-savehist-unpropertize-variables-h)

  (defun thomas-savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."

    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
                (cl-remove-if-not #'savehist-printable register-alist)))
  (add-hook 'savehist-save-hook #'thomas-savehist-remove-unprintable-registers-h))


;; 记录关闭之前打开 Buffer 光标位置
(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode)
  :custom (save-place-file (expand-file-name "saveplace" thomas-cache-dir)))

;;
;;; jump
(use-package better-jumper
  :hook (after-init . better-jumper-mode)
  :commands thomas-set-jump-a thomas-set-jump-maybe-a thomas-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (with-eval-after-load 'evil
    (general-def
      [remap evil-jump-forward]  'better-jumper-jump-forward
      [remap evil-jump-backward] 'better-jumper-jump-backward
      [remap xref-pop-marker-stack] 'better-jumper-jump-backward))
  (general-def
    [remap xref-go-back] 'better-jumper-jump-backward
    [remap xref-go-forward] 'better-jumper-jump-forward)
  :config
  :config
  (defun thomas-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun thomas-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

  (defun thomas-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'thomas-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'thomas-set-jump-a))


;;
;;; Edit
;; 智能添加 parens 符号
(use-package smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  :hook (find-file . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  ;; smartparens recognizes `slime-mrepl-mode', but not `sly-mrepl-mode', so...
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil)
    ;; Smartparens conditional binds a key to C-g when sp overlays are active
    ;; (even if they're invisible). This disruptively changes the behavior of
    ;; C-g in insert mode, requiring two presses of the key to exit insert mode.
    ;; I don't see the point of this keybind, so...
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (defun thomas-init-smartparens-in-eval-expression-h ()
    "Enable `smartparens-mode' in the minibuffer for `eval-expression'.
This includes everything that calls `read--expression', e.g.
`edebug-eval-expression' Only enable it if
`smartparens-global-mode' is on."
    (when smartparens-global-mode (smartparens-mode +1)))
  (add-hook 'eval-expression-minibuffer-setup-hook #'thomas-init-smartparens-in-eval-expression-h)

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil))

;; 删除末尾空白
(use-package ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :hook (after-init . ws-butler-mode)
  :config
  ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil))

;;
;;; Motion
(use-package avy
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil))

;;
;;; Server
;; Use emacsclient to connect
(use-package server
  :when (display-graphic-p)
  :straight (:type built-in)
  :hook (after-init . server-mode)
  :config
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  (unless (server-running-p)
    (server-start)))

;; transparent remote access
(use-package tramp
  :straight (:type built-in)
  :defer t
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-completion-reread-directory-timeout 60
        tramp-verbose 1
        tramp-default-method "ssh"
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")))

(provide 'init-editor)
;;; init-editor.el ends here
